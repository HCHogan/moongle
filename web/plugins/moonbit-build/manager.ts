import type { ViteDevServer } from 'vite'
import { watch } from 'chokidar'
import { checkStale } from './file-utils'
import { createLogger, ensureCli, ensurePublicWasmPresent, buildOnce, createRebuildScheduler } from './build-utils'
import type { MoonbitTsOptions, BuildState } from './types'

/**
 * 核心构建管理器类
 */
export class MoonbitBuildManager {
  private state: BuildState
  private options: Required<MoonbitTsOptions>
  private log: (msg: string) => void
  private scheduleRebuild?: (reason: string) => void

  constructor(options: MoonbitTsOptions) {
    this.options = {
      repoPath: 'vendor/tree-sitter-moonbit',
      outName: 'tree-sitter-moonbit.wasm',
      publicDir: '',
      watch: true,
      cliVersion: '0.25.8',
      debounceMs: 120,
      devSkipBuild: false,
      forceBuildOnProd: false,
      ...options
    }

    this.state = {
      viteRoot: '',
      resolvedPublicDir: '',
      server: undefined,
      debounceTimer: null,
      building: false,
      isDev: false
    }

    this.log = createLogger('moonbit')
  }

  /**
   * 配置解析时的初始化
   */
  configResolved(viteRoot: string, publicDir: string, isDev: boolean) {
    this.state.viteRoot = viteRoot
    this.state.resolvedPublicDir = this.options.publicDir || publicDir
    this.state.isDev = isDev

    this.log(`root=${viteRoot}`)
    this.log(`publicDir=${this.state.resolvedPublicDir}`)
    this.log(`mode=${isDev ? 'dev(serve)' : 'build'}`)
  }

  /**
   * 配置开发服务器
   */
  configureServer(server: ViteDevServer) {
    this.state.server = server

    if (!this.options.watch) return

    // 创建重建调度器
    this.scheduleRebuild = createRebuildScheduler(
      () => this.ensureFreshWasm(false),
      this.options.debounceMs,
      this.log
    )

    // 监听源文件变化
    const repoPath = this.getRepoPath()
    const watchPaths = [
      `${repoPath}/grammar.js`,
      `${repoPath}/queries/**/*.scm`,
      `${repoPath}/src/**/*.js`
    ]

    const watcher = watch(watchPaths, { ignoreInitial: true })
    
    watcher.on('change', (path) => {
      if (this.state.isDev && this.options.devSkipBuild && !process.env.MOONBIT_BUILD_WASM) {
        this.log(`检测到变化但跳过按需构建（devSkipBuild=true）。需要立即重建可设 MOONBIT_BUILD_WASM=1`)
        return
      }
      this.log(`检测到变化：${path}`)
      this.scheduleRebuild?.(path)
    })

    // 服务器关闭时清理
    server.httpServer?.on('close', () => {
      watcher.close()
    })
  }

  /**
   * 构建开始时的处理
   */
  async buildStart(): Promise<void> {
    await this.ensurePublicWasmPresent()
    
    if (this.state.isDev) {
      if (this.options.devSkipBuild && !process.env.MOONBIT_BUILD_WASM) {
        this.log('开发模式跳过按需构建，使用 public 里的现成 wasm')
        return
      }
      await this.ensureFreshWasm(false)
    } else {
      await this.ensureFreshWasm(this.options.forceBuildOnProd)
    }
  }

  /**
   * 获取仓库路径
   */
  private getRepoPath(): string {
    return `${this.state.viteRoot}/${this.options.repoPath}`
  }

  /**
   * 获取仓库中的 wasm 路径
   */
  private getWasmInRepo(): string {
    return `${this.getRepoPath()}/${this.options.outName}`
  }

  /**
   * 获取 public 中的 wasm 路径
   */
  private getWasmInPublic(): string {
    return `${this.state.resolvedPublicDir}/${this.options.outName}`
  }

  /**
   * 确保 public 目录有 wasm 文件
   */
  private async ensurePublicWasmPresent(): Promise<void> {
    await ensurePublicWasmPresent(
      this.getWasmInRepo(),
      this.getWasmInPublic(),
      this.log
    )
  }

  /**
   * 按需构建（如果缺失或过期才编），支持强制
   */
  private async ensureFreshWasm(force = false): Promise<void> {
    if (this.state.building) return

    const repoPath = this.getRepoPath()
    const wasmInPublic = this.getWasmInPublic()

    const staleCheck = force 
      ? { stale: true, reason: 'force' } 
      : checkStale(repoPath, wasmInPublic)

    if (!staleCheck.stale) {
      this.log(`跳过构建：${staleCheck.reason}`)
      return
    }

    this.state.building = true
    try {
      this.log(`按需构建：${staleCheck.reason}`)
      await ensureCli(this.options.cliVersion)
      
      const result = await buildOnce(
        repoPath,
        this.getWasmInRepo(),
        wasmInPublic,
        this.log
      )

      if (result.success) {
        this.state.server?.ws.send({ type: 'full-reload' })
      } else {
        this.log(`构建失败: ${result.error}`)
      }
    } finally {
      this.state.building = false
    }
  }
}