import { existsSync, statSync, readdirSync } from 'node:fs'
import { join } from 'node:path'
import type { FileStats, StaleCheck } from './types'

/**
 * 递归收集源码文件（只取：grammar.js / queries/*.scm / src/**/*.js）
 */
export function collectSourceFiles(dir: string): string[] {
  const res: string[] = []
  const qDir = join(dir, 'queries')
  const sDir = join(dir, 'src')
  const gFile = join(dir, 'grammar.js')
  
  if (existsSync(gFile)) res.push(gFile)

  const walk = (d: string, exts: string[]) => {
    if (!existsSync(d)) return
    const stack = [d]
    
    while (stack.length) {
      const cur = stack.pop()!
      for (const name of readdirSync(cur, { withFileTypes: true })) {
        const p = join(cur, name.name)
        if (name.isDirectory()) {
          // 忽略 node_modules、.git、生成物目录
          if (['node_modules', '.git', 'build', 'dist'].includes(name.name)) continue
          stack.push(p)
        } else {
          const ext = p.split('.').pop() || ''
          if (exts.includes(ext)) res.push(p)
        }
      }
    }
  }
  
  walk(qDir, ['scm'])
  walk(sDir, ['js'])
  return res
}

/**
 * 获取文件的修改时间
 */
export function getMtime(path: string): number {
  try { 
    return statSync(path).mtimeMs 
  } catch { 
    return 0 
  }
}

/**
 * 判断是否需要重编：wasm 不存在或任一源码 mtime > wasm mtime
 */
export function checkStale(repoPath: string, wasmPath: string): StaleCheck {
  if (!existsSync(wasmPath)) {
    return { stale: true, reason: 'wasm 文件不存在' }
  }

  if (!existsSync(repoPath)) {
    return { stale: false, reason: '源码目录不存在' }
  }

  const sourceFiles = collectSourceFiles(repoPath)
  if (sourceFiles.length === 0) {
    return { stale: false, reason: '无源码文件' }
  }

  const wasmMtime = getMtime(wasmPath)
  const fileStats: FileStats[] = sourceFiles.map(path => ({
    path,
    mtime: getMtime(path)
  }))

  const newerFiles = fileStats.filter(f => f.mtime > wasmMtime)
  if (newerFiles.length > 0) {
    return { 
      stale: true, 
      reason: `源码更新 (${newerFiles.length} 个文件)` 
    }
  }

  return { stale: false, reason: 'wasm 是最新的' }
}

/**
 * 获取所有源文件的统计信息
 */
export function getSourceFileStats(repoPath: string): FileStats[] {
  if (!existsSync(repoPath)) return []
  
  const sourceFiles = collectSourceFiles(repoPath)
  return sourceFiles.map(path => ({
    path,
    mtime: getMtime(path)
  }))
}