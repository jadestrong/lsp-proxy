# Ivy 符号搜索

`lsp-proxy-ivy.el` 提供当前项目的动态 workspace symbol 搜索。它使用已有的 Ivy 和 lsp-proxy，不依赖 lsp-mode、lsp-protocol 或 lsp-ivy。

## 启用与命令

Ivy 适配默认不加载，也不由主模块自动注册命令。使用 Ivy 的用户自行在配置中显式加载：

```elisp
(require 'lsp-proxy-ivy)
```

| 命令 | 用途 |
| --- | --- |
| `M-x lsp-proxy-ivy-workspace-symbol` | 动态搜索当前文档关联项目的符号 |
| `C-u M-x lsp-proxy-ivy-workspace-symbol` | 用光标处符号预填输入 |
| `M-x lsp-proxy-ivy-spring-running-endpoints` | 用 `//` 预填，搜索已连接运行应用的 endpoint |

在启用 `lsp-proxy-mode` 且服务器已就绪的源 buffer 中调用。这个模块只增加 Emacs 界面；已有 workspace-symbol 后端可直接使用，无需重新构建 Rust 或重新安装 JDT LS/STS4。

## 输入与选择

参考用户提供的 `lsp-ivy.el`：第一个词发送给服务器，其余词对返回候选做不区分大小写的字面量过滤。只有第一个词改变时才重新查询，默认等待 200 ms。空输入默认不请求服务器。

| 输入示例 | 服务器查询 | 本地筛选 |
| --- | --- | --- |
| `OrderService` | `OrderService` | 无 |
| `@/ /api` | `@/` | `/api` |
| `@+ OrderService` | `@+` | `OrderService` |
| `@> transform` | `@>` | `transform` |
| `// /one` | `//` | `/one` |

候选显示符号名称、类型、项目相对路径或 URI、来源服务器，并保留同名结果。选中源码符号时复用已有的 resolve 和位置导航；HTTP/HTTPS 结果通过 `browse-url` 打开。

支持 `ivy-resume`：原 buffer 存活时可继续搜索，输入变化后会重新请求。源 buffer 已关闭时需要从有效文档重新发起搜索。

运行 endpoint 仍需要先通过 `lsp-proxy-java-spring-connect-running-app` 连接应用的 LiveData；服务器配置见 [Java 文档](java.md)。

## 可配置项

| 变量 | 默认值 | 用途 |
| --- | --- | --- |
| `lsp-proxy-ivy-show-symbol-kind` | `t` | 显示符号类型 |
| `lsp-proxy-ivy-show-symbol-filename` | `t` | 显示源文件路径或 URI |
| `lsp-proxy-ivy-filter-symbol-kind` | `nil` | 排除指定的 LSP SymbolKind 数字 |
| `lsp-proxy-ivy-debounce-delay` | `0.2` | 查询防抖秒数 |
| `lsp-proxy-ivy-minimum-query-length` | `1` | 第一个词的最小长度；设为 0 可查询空字符串 |

关闭选择器或源 buffer 时清理计时器和请求。旧请求会发出取消通知，即便服务器忽略取消，过期回包也不会覆盖新候选。搜索失败或超时会清空当前结果并显示消息。

此命令使用当前文档的服务器关联，不提供全局 workspace 搜索或文件夹管理。原有 `lsp-proxy-workspace-symbol` 和 xref apropos 继续可用。
