// Types for Moonbit tree-sitter plugin
export interface MoonbitTsOptions {
  repoPath?: string               // 相对 Vite root，例如 'vendor/tree-sitter-moonbit'
  outName?: string                // 输出 wasm 文件名，默认 'tree-sitter-moonbit.wasm'
  publicDir?: string              // 不填用 Vite 的 publicDir
  watch?: boolean                 // dev 监听源文件
  cliVersion?: string             // tree-sitter-cli 版本（与 web-tree-sitter ABI 一致）
  debounceMs?: number             // 监听防抖
  devSkipBuild?: boolean          // dev 是否跳过"按需编译"（默认 false=按需编）
  forceBuildOnProd?: boolean      // 生产是否强制重编（默认 false=按需）
}

export interface BuildState {
  viteRoot: string
  resolvedPublicDir: string
  server?: import('vite').ViteDevServer
  debounceTimer: NodeJS.Timeout | null
  building: boolean
  isDev: boolean
}

export interface PathConfig {
  repoPath: string
  outName: string
  publicDir?: string
}

export interface BuildResult {
  success: boolean
  reason?: string
  error?: string
}

export interface FileStats {
  path: string
  mtime: number
}

export interface StaleCheck {
  stale: boolean
  reason: string
}