import { exec as _exec } from 'node:child_process'
import { promisify } from 'node:util'
import { existsSync, mkdirSync, copyFileSync } from 'node:fs'
import { dirname } from 'node:path'
import type { BuildResult } from './types'

const exec = promisify(_exec)

/**
 * 日志工具函数
 */
export function createLogger(prefix = 'moonbit') {
  return (message: string) => {
    console.log(`\x1b[36m[${prefix}]\x1b[0m ${message}`)
  }
}

/**
 * 执行命令工具函数
 */
export async function runCommand(cmd: string, cwd: string, log: (msg: string) => void): Promise<void> {
  log(`$ ${cmd}  (cwd: ${cwd})`)
  const { stdout, stderr } = await exec(cmd, { cwd })
  if (stdout?.trim()) process.stdout.write(stdout)
  if (stderr?.trim()) process.stderr.write(stderr)
}

/**
 * 确保 tree-sitter-cli 已安装
 */
export async function ensureCli(cliVersion: string): Promise<void> {
  try { 
    await exec('npx --yes tree-sitter --version') 
  } catch {
    throw new Error(
      `未检测到 tree-sitter-cli。请先安装：\n` +
      `  npm i -D tree-sitter-cli@${cliVersion}\n` +
      `或  pnpm add -D tree-sitter-cli@${cliVersion}`
    )
  }
}

/**
 * 确保 public 目录存在 wasm 文件（从可能的地方复制）
 */
export async function ensurePublicWasmPresent(
  wasmInRepo: string, 
  wasmInPublic: string, 
  log: (msg: string) => void
): Promise<void> {
  if (existsSync(wasmInPublic)) {
    log(`public wasm 已存在：${wasmInPublic}`)
    return
  }

  if (existsSync(wasmInRepo)) {
    mkdirSync(dirname(wasmInPublic), { recursive: true })
    copyFileSync(wasmInRepo, wasmInPublic)
    log(`已复制现有 wasm 到 public：${wasmInPublic}`)
  } else {
    log(`public 和 repo 都缺少 wasm，将触发构建`)
  }
}

/**
 * 执行单次构建
 */
export async function buildOnce(
  repoPath: string,
  wasmInRepo: string,
  wasmInPublic: string,
  log: (msg: string) => void
): Promise<BuildResult> {
  try {
    if (!existsSync(repoPath)) {
      const error = `未找到语法仓库：${repoPath}（请确认 submodule）`
      throw new Error(error)
    }

    await runCommand('npx tree-sitter build --wasm', repoPath, log)
    
    if (!existsSync(wasmInRepo)) {
      const error = `未生成 wasm：${wasmInRepo}。检查 grammar 与 CLI 版本/ABI。`
      throw new Error(error)
    }

    // 确保目标目录存在
    mkdirSync(dirname(wasmInPublic), { recursive: true })
    copyFileSync(wasmInRepo, wasmInPublic)
    log(`wasm 已复制到：${wasmInPublic}`)

    return { success: true, reason: 'build completed' }
  } catch (error) {
    const errorMsg = error instanceof Error ? error.message : String(error)
    log(`构建失败: ${errorMsg}`)
    return { success: false, error: errorMsg }
  }
}

/**
 * 创建防抖的重建调度器
 */
export function createRebuildScheduler(
  callback: () => Promise<void>,
  debounceMs: number,
  log: (msg: string) => void
): (reason: string) => void {
  let debounceTimer: NodeJS.Timeout | null = null

  return (reason: string) => {
    if (debounceTimer) clearTimeout(debounceTimer)
    debounceTimer = setTimeout(async () => {
      try {
        log(`触发重建: ${reason}`)
        await callback()
      } catch (error) {
        log(`重建失败: ${error}`)
      }
    }, debounceMs)
  }
}