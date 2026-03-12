const childProcess = require('child_process')
const fs = require('fs/promises')

class SnapshotTestRunner {
  constructor(cliBinPath, mode) {
    this.cliBinPath = cliBinPath;
    this.mode = mode;
  }

  async runTests(tests, printExpectedAndActual, updateSnapshotWhenFailed) {
    const results = []
    for (const { test, assertions, printModulesOnErr = false } of tests) {
      const result = await this.runTest(test, assertions, printModulesOnErr, updateSnapshotWhenFailed)
      results.push(result)
    }

    return outputResults(results, printExpectedAndActual)
  }

  async runTest(testFile, outputFile, printModulesOnErr, updateSnapshotWhenFailed) {
    const testFilePath = `${__dirname}/${testFile}`
    const outputFilePath = `${__dirname}/${outputFile}`

    try {
      const cmdArgs = ['emit-ir', '--mode']
      switch (this.mode) {
        default:
          throw new Error(`Invalid mode '${this.mode}'`)
        case 'TOKENS_ONLY':
        case 'AST':
          cmdArgs.push(this.mode)
          break
        case 'TYPED_AST':
          cmdArgs.push(this.mode)
          if (printModulesOnErr) {
            cmdArgs.push('--ignore-errors')
          }
          break;
      }
      cmdArgs.push(testFilePath)

      const [actual, expectedOutput] = await Promise.all([
        runCommand(this.cliBinPath, cmdArgs),
        fs.readFile(outputFilePath, { encoding: 'utf8' }),
      ])

      const expected = expectedOutput
        .replaceAll('%FILE_NAME%', testFilePath)
        .replaceAll('%TEST_DIR%', __dirname)

      if (actual !== expected) {
        if (updateSnapshotWhenFailed) {
          const updatedFile = actual.replaceAll(__dirname, '%TEST_DIR%')
          await fs.writeFile(outputFilePath, updatedFile, { encoding: 'utf-8' })
          return { status: 'updated', testFile }
        }
        return { status: 'fail', testFile, expected, actual }
      }

      return { status: 'pass', testFile }
    } catch (error) {
      return { status: 'error', testFile, error }
    }
  }
}

class InlineTestRunner {
  constructor(cliBinPath, mode) {
    this.cliBinPath = cliBinPath;
    this.mode = mode;
  }

  async runTests(tests, printExpectedAndActual) {
    const results = []
    for (const { test, args, env } of tests) {
      const result = await this.runTest(test, args, env)
      results.push(result)
    }

    return outputResults(results, printExpectedAndActual)
  }

  async runTest(testFile, args = [], env = {}) {
    const testFilePath = `${__dirname}/${testFile}`

    try {
      const [actual, expectedOutput] = await Promise.all([
        this.buildAndRunTest(testFilePath, args, env),
        fs.readFile(testFilePath, { encoding: 'utf8' }),
      ])

      const stdDir = process.env.ABRA_STD ? process.env.ABRA_STD : `${process.env.ABRA_ROOT}/std`
      const re = /^\s*\/\/\/ Expect: (.*)$/
      const expectations = expectedOutput.split('\n')
        .map((line, idx) => {
          const match = re.exec(line)
          if (!match) return null

          const expectation = match[1]
            .replaceAll('%TEST_DIR%', __dirname)
            .replaceAll('%STD_DIR%', stdDir)
          return [idx + 1, expectation]
        })
        .filter(line => !!line)

      const actualLines = actual.trimEnd().split('\n')
      let i = 0
      for (; i < actualLines.length; i++) {
        const actual = actualLines[i]
        const expected = expectations[i]
        if (!expected || actual !== expected[1]) {
          return { status: 'fail', testFile, expected, actual }
        }
      }

      return { status: 'pass', testFile }
    } catch (error) {
      return { status: 'error', testFile, error }
    }
  }

  async buildAndRunTest(testFilePath, args, env) {
    const testPathSegs = testFilePath.split('/')
    const testName = testPathSegs[testPathSegs.length - 1].replace('.abra', '')

    switch (this.mode) {
      case 'js': {
        const testMjsPath = `${process.cwd()}/.abra/${testName}.mjs`
        const jsWrapperPath = `${process.cwd()}/test/js_harness_node.mjs`

        await runCommand(this.cliBinPath, ['build', '-t', 'js', testFilePath])
        return runCommand('node', [jsWrapperPath, testMjsPath, '--', ...args], env)
      }
      case 'vm': {
        return runCommand(this.cliBinPath, [testFilePath, ...args], env)
      }
      case 'compiler': {
        await runCommand(this.cliBinPath, ['build', '-t', 'bin', testFilePath])
        return runCommand(`${process.cwd()}/.abra/${testName}`, args, env)
      }
      default: throw new Error(`Unsupported mode ${this.mode}`)
    }
  }
}

function outputResults(results, printExpectedAndActual) {
  let numPass = 0
  let numFail = 0
  let numErr = 0
  let numUpdated = 0

  for (const result of results) {
    switch (result.status) {
      case 'pass': {
        numPass += 1
        console.log(green(`  [PASS] ${result.testFile}`))
        break
      }
      case 'fail': {
        numFail += 1
        console.log(magenta(`  [FAIL] ${result.testFile}`))
        if (printExpectedAndActual) {
          console.log('EXPECTED')
          console.log(result.expected)
          console.log('ACTUAL')
          console.log(result.actual)
        }
        break
      }
      case 'error': {
        numErr += 1
        console.log(red(`  [ERROR] ${result.testFile}`))

        const errFmt = result.error.toString().split('\n').map(line => `    ${line}`).join('\n')
        console.log(red(errFmt))
        break
      }
      case 'updated': {
        numUpdated += 1
        console.log(yellow(`  [UPDATED] ${result.testFile}`))
        break
      }
    }
  }

  console.log()
  const passMsg = `  Pass: ${numPass} / ${results.length}`
  console.log(numPass === results.length ? green(passMsg) : passMsg)
  const failMsg = `  Fail: ${numFail} / ${results.length}`
  console.log(numFail > 0 ? magenta(failMsg) : failMsg)
  const errMsg = `  Error: ${numErr} / ${results.length}`
  console.log(numErr > 0 ? magenta(errMsg) : errMsg)
  const updatedMsg = `  Updated: ${numUpdated} / ${results.length}`
  console.log(numUpdated > 0 ? yellow(updatedMsg) : updatedMsg)

  return { numPass, numFail, numErr, numUpdated }
}

function runCommand(command, args, envVars = {}) {
  const env = { ...process.env, ...envVars }
  const cmd = childProcess.spawn(command, args, { env })
  return new Promise((res, rej) => {
    let stdoutBuf = ''
    cmd.stdout.on('data', data => { stdoutBuf += data })

    let stderrBuf = ''
    cmd.stderr.on('data', data => { stderrBuf += data })

    cmd.on('exit', code => code === 0 ? res(stdoutBuf) : rej(stdoutBuf + '\n' + stderrBuf))

    cmd.on('error', err => rej(err))
  })
}

const colors = {
  reset: "\x1b[0m",
  black: "\x1b[30m",
  red: "\x1b[31m",
  green: "\x1b[32m",
  yellow: "\x1b[33m",
  blue: "\x1b[34m",
  magenta: "\x1b[35m",
  cyan: "\x1b[36m",
  white: "\x1b[37m",
  gray: "\x1b[90m",
  crimson: "\x1b[38m"
}

const red = str => `${colors.red}${str}${colors.reset}`
const green = str => `${colors.green}${str}${colors.reset}`
const yellow = str => `${colors.yellow}${str}${colors.reset}`
const magenta = str => `${colors.magenta}${str}${colors.reset}`

module.exports = { SnapshotTestRunner, InlineTestRunner, runCommand, red, yellow, green, magenta }
