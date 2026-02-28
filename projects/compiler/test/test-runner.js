const childProcess = require('child_process')
const fs = require('fs/promises')

class TestRunner {
  constructor(runnerName, harnessPath, { target = 'native', cliMode = null } = {}) {
    this.runnerName = runnerName
    this.harnessPath = harnessPath
    this.target = target
    this.cliMode = cliMode
  }

  async runTests(tests, updateSnapshotWhenFailed) {
    console.log(`Running test suite ${this.runnerName}:`)
    try {
      if (this.cliMode) {
        console.log(`  Compiling cli '${this.harnessPath}'`)
        this.runnerName += '_cli'
      } else {
        console.log(`  Compiling test harness '${this.harnessPath}'`)
      }
      await runCommand('abra', ['build', '-o', this.runnerName, this.harnessPath])
      console.log('  Done\n')
    } catch (err) {
      console.log(red('  Failed to compile test harness:'))
      const errFmt = err.toString().split('\n').map(line => `    ${line}`).join('\n')
      console.log(red(errFmt))
      return { numPass: 0, numFail: 0, numErr: tests.length }
    }
    const runnerBin = `${process.cwd()}/._abra/${this.runnerName}`

    const results = []
    for (const { test, assertions, args, env, printModulesOnErr = false } of tests) {
      let result
      if (!!assertions) {
        result = await this._runTest(runnerBin, test, assertions, printModulesOnErr, updateSnapshotWhenFailed)
      } else {
        result = await this._runCompilerTest(runnerBin, test, args, env)
      }
      results.push(result)
    }

    return this._outputResults(results)
  }

  async _runTest(bin, testFile, outputFile, printModulesOnErr, updateSnapshotWhenFailed) {
    const testFilePath = `${__dirname}/${testFile}`
    const outputFilePath = `${__dirname}/${outputFile}`

    try {
      const cmdArgs = ['emit-ir', '--mode']
      switch (this.cliMode) {
        default:
          throw new Error(`Invalid cli mode '${this.cliMode}'`)
        case 'TOKENS_ONLY':
        case 'AST':
          cmdArgs.push(this.cliMode)
          break
        case 'TYPED_AST':
          cmdArgs.push(this.cliMode)
          if (printModulesOnErr) {
            cmdArgs.push('--ignore-errors')
          }
          break;
      }
      cmdArgs.push(testFilePath)

      const [actual, expectedOutput] = await Promise.all([
        runCommand(bin, cmdArgs),
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

  async _runCompilerTest(bin, testFile, args = [], env = {}) {
    const testFilePath = `${__dirname}/${testFile}`

    async function buildAndRunTestBin(testFilePath, compilerBin, args, env, target) {
      const testPathSegs = testFilePath.split('/')
      const testName = testPathSegs[testPathSegs.length - 1].replace('.abra', '')

      if (target === 'js') {
        const testMjsPath = `${process.cwd()}/._abra/${testName}.mjs`
        const jsWrapperPath = `${process.cwd()}/test/js_harness_node.mjs`

        await fs.writeFile(testMjsPath, '', { encoding: 'utf-8' })
        await runCommand(compilerBin, [testFilePath, testName])
        return runCommand('node', [jsWrapperPath, testMjsPath, '--', ...args], env)
      } else if (target === 'vm') {
        return runCommand(compilerBin, [testFilePath, ...args], env)
      } else if (target === 'native') {
        await runCommand('abra', ['build', '-o', testName, testFilePath], { COMPILER_BIN: compilerBin })
        return runCommand(`${process.cwd()}/._abra/${testName}`, args, env)
      } else {
        throw new Error(`Unsupported target ${target}`)
      }
    }

    try {
      const [actual, expectedOutput] = await Promise.all([
        buildAndRunTestBin(testFilePath, bin, args, env, this.target),
        fs.readFile(testFilePath, { encoding: 'utf8' }),
      ])

      const re = /^\s*\/\/\/ Expect: (.*)$/
      const expectations = expectedOutput.split('\n')
        .map((line, idx) => {
          const match = re.exec(line)
          if (!match) return null

          const expectation = match[1]
            .replaceAll('%TEST_DIR%', __dirname)
            .replaceAll('%STD_DIR%', process.env.ABRA_HOME)
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

  _outputResults(results) {
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
          // console.log('EXPECTED')
          // console.log(result.expected)
          // console.log('ACTUAL')
          // console.log(result.actual)
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

module.exports = { TestRunner, red, yellow, green, magenta }
