const childProcess = require('child_process')
const fs = require('fs/promises')

class TestRunner {
  constructor(runnerName, harnessPath) {
    this.runnerName = runnerName
    this.harnessPath = harnessPath
  }

  async runTests(tests) {
    console.log(`Running tests for ${this.runnerName}:`)
    const runnerBin = `${process.cwd()}/._abra/${this.runnerName}`
    try {
      console.log(`  Compiling test harness '${this.harnessPath}'\n`)
      await runCommand('abra', ['build', '-o', this.runnerName, this.harnessPath])
    } catch (err) {
      console.log('  Failed to compile test harness:')
      const errFmt = err.toString().split('\n').map(line => `    ${line}`).join('\n')
      console.log(errFmt)
      return { numPass: 0, numFail: 0, numErr: tests.length }
    }

    const results = []
    for (const { test, assertions, args, env } of tests) {
      const result = !!assertions
        ? await this._runTest(runnerBin, test, assertions)
        : await this._runCompilerTest(runnerBin, test, args, env)
      results.push(result)
    }

    return this._outputResults(results)
  }

  async _runTest(lexerBin, testFile, outputFile) {
    const testFilePath = `${__dirname}/${testFile}`
    const outputFilePath = `${__dirname}/${outputFile}`

    try {
      const [actual, expectedOutput] = await Promise.all([
        runCommand(lexerBin, [testFilePath]),
        fs.readFile(outputFilePath, { encoding: 'utf8' }),
      ])

      const expected = expectedOutput
        .replaceAll('%FILE_NAME%', testFilePath)
        .replaceAll('%TEST_DIR%', __dirname)

      if (actual !== expected) {
        return { status: 'fail', testFile, expected, actual }
      }

      return { status: 'pass', testFile }
    } catch (error) {
      return { status: 'error', testFile, error }
    }
  }

  async _runCompilerTest(lexerBin, testFile, args = [], env = {}) {
    const testFilePath = `${__dirname}/${testFile}`

    try {
      const [actual, expectedOutput] = await Promise.all([
        runCommand('abra', [testFilePath, ...args], { COMPILER_BIN: lexerBin, ...env }),
        fs.readFile(testFilePath, { encoding: 'utf8' }),
      ])

      const re = /^\s*\/\/\/ Expect: (.*)$/
      const expectations = expectedOutput.split('\n')
        .map((line, idx) => {
          const match = re.exec(line)
          if (!match) return null

          const expectation = match[1]
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

    for (const result of results) {
      switch (result.status) {
        case 'pass': {
          numPass += 1
          console.log(`  [PASS] ${result.testFile}`)
          break
        }
        case 'fail': {
          numFail += 1
          console.log(`  [FAIL] ${result.testFile}`)
          console.log('EXPECTED')
          console.log(result.expected)
          console.log('ACTUAL')
          console.log(result.actual)
          break
        }
        case 'error': {
          numErr += 1
          console.log(`  [ERROR] ${result.testFile}`)

          const errFmt = result.error.toString().split('\n').map(line => `    ${line}`).join('\n')
          console.log(errFmt)
          break
        }
      }
    }

    console.log()
    console.log(`  Pass: ${numPass} / ${results.length}`)
    console.log(`  Fail: ${numFail} / ${results.length}`)
    console.log(`  Error: ${numErr} / ${results.length}`)

    return { numPass, numFail, numErr }
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

module.exports = { TestRunner }
