# LSP-Proxy Remote Development Usage Guide

## 修复的问题

修复了 `lsp-proxy-remote.el` 中请求格式与后端不兼容的问题：

### 问题描述
- **错误**: `(invalid-function lsp-proxy--request)`
- **根因**: `lsp-proxy-remote.el` 尝试调用 `lsp-proxy.el` 内部函数，但这些函数可能未加载
- **解决方案**: 手动构造后端期望的请求格式

### 修复内容

1. **请求函数重构**:
   - 不再依赖 `lsp-proxy--request` 函数
   - 直接使用 `jsonrpc-request` 和 `jsonrpc-async-request`
   - 手动构造后端期望的参数格式

2. **参数格式修复**:
   ```elisp
   ;; 现在的格式
   (list :uri nil :params actual-parameters)
   ```
   这匹配后端期望的结构：
   ```rust
   // 后端代码
   let params = serde_json::from_value(req.params.params.clone())?;
   ```

## 使用示例

### 1. 启用远程模式
```elisp
(require 'lsp-proxy-remote)
(lsp-proxy-remote-mode 1)
```

### 2. 列出可用服务器
```elisp
M-x lsp-proxy-remote-list-servers
```

### 3. 连接到远程服务器
```elisp
M-x lsp-proxy-remote-connect
```
- 会自动调用 `emacs/remoteList` 获取真实服务器列表
- 用户从实际可用服务器中选择
- 支持覆盖配置（host/user/port）

### 4. 查看服务器状态
```elisp
M-x lsp-proxy-remote-status
```

### 5. 远程文件操作
```elisp
M-x lsp-proxy-remote-open-file  ; 打开远程文件
M-x lsp-proxy-remote-save-file  ; 保存远程文件
```

## 键位绑定
在 `lsp-proxy-remote-mode` 中可用：

- `C-c r c` - 连接服务器
- `C-c r d` - 断开服务器  
- `C-c r l` - 列出服务器
- `C-c r s` - 服务器状态
- `C-c r o` - 打开远程文件
- `C-c r w` - 列出工作区
- `C-c r r` - 刷新服务器列表
- `C-c r S` - 保存远程文件

## 注意事项

1. **依赖关系**: `lsp-proxy-remote.el` 现在可以独立编译，但运行时仍需要 `lsp-proxy.el`
2. **连接状态**: 使用 `lsp-proxy-remote--connection-alivep-safe()` 安全检查连接
3. **错误处理**: 改进了错误处理和用户反馈
4. **服务器列表**: 现在从后端动态获取，不再依赖硬编码列表

## 测试验证

修复后，以下操作应该正常工作：
- ✅ 编译不报错
- ✅ 加载模块不报 `invalid-function` 错误  
- ✅ `lsp-proxy-remote-connect` 能正确获取服务器列表
- ✅ 所有远程请求使用正确的参数格式