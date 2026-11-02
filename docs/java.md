# Java Support

LSP-Proxy ships two Java backends:

| Backend | Role |
| --- | --- |
| `intellij` | Default Java backend. Also serves Kotlin. |
| `jdtls` | Eclipse JDT Language Server, available as an alternative Java backend. |

## IntelliJ Backend

### Install

Use the managed installer:

```elisp
M-x lsp-proxy-java-install-server
```

Choose `intellij`, or press enter to use the default. The managed install puts
the backend's `bin/` directory on the proxy process `PATH`, so
`command = "intellij-server"` is enough.

| Setting | Purpose |
| --- | --- |
| `lsp-proxy-java-server-version` | IntelliJ server build to install. `nil` installs the latest known build. |
| Project requirement | A Maven, Gradle, or Bazel project root with the corresponding build tool on `PATH`. |

### Configuration Fields

| Field | Default/example | Purpose |
| --- | --- | --- |
| `command` | `"intellij-server"` | Starts the headless backend directly over stdio. |
| `args` | `["--stdio", "--eula=34d850193ee04897"]` | Launcher flags. `--eula` is the accepted hash of the bundle's own `EULA.txt`. |
| `environment.IJ_JAVA_OPTIONS` | `"-Xmx4g"` | JVM options for the backend. |
| `environment.INTELLIJ_DATA_SHARING` | `"none"` | Optional data-sharing mode; equivalent to `--data-sharing`. |
| `environment.INTELLIJ_REGION` | `"americas"` | Optional region; equivalent to `--region`. |
| `config.intellijExtensions` | `true` | Enables IntelliJ-specific protocol extensions used by LSP-Proxy. |
| `config.defaultSdk` | `""` | Optional fallback SDK for symbol resolution. Must be a modular JDK, Java 9+ with `lib/modules`. |
| `config.buildTools` | `{}` | Lets the backend use default Maven, Gradle, and Bazel discovery. |
| `config.projects` | `[]` | Optional explicit project list. Entries may use `type = "gradle"`, `"maven"`, `"bazel"`, `"json"`, or `"subproject"`. |
| `config.disableRocksDBWriteAheadLog` | commented | Optional backend knob; omit to use the backend default. |
| `config.runMainCodeLens` | `true` | Emits Run/Debug CodeLens above Java `main` methods. |

Useful launcher flags:

```toml
args = [
  "--stdio",
  "--eula=34d850193ee04897",
  "--region=americas",
  "--data-sharing=none",
  "--system-path=/path/to/cache",
  "--log-level=INFO",
]
```

| Launcher option | Values |
| --- | --- |
| `--region` | `africa`, `americas`, `apac`, `china`, `europe`, `middle_east`, `oceania`, `not_set` |
| `--data-sharing` | `full`, `anonymous`, `test`, `none` |
| `--system-path` | Cache/index directory |
| `--ca-certificate` | Extra trusted CA file, PEM or DER |
| `--log-level` | `TRACE`, `DEBUG`, `INFO`, `WARNING`, `ERROR`, `OFF`, `ALL` |

### Supported Methods

| Method/capability | What LSP-Proxy does |
| --- | --- |
| `decompile` | Fetches source for `jar:` and `jrt:` virtual locations and serves them through read-only buffers. |
| `interpolateFileTemplate` | Fills newly created JVM source files from IntelliJ templates. |
| `intellij/importLog` | Reports Maven, Gradle, or Bazel import output. |
| `intellij/chooseAction` | Handles ModCommand code actions that require choosing one variant. |
| `intellij/runEditorCommand` | Lets the backend ask Emacs to run local editor commands such as rename. |
| `intellij.java.resolveLaunch` | Resolves Java run/debug launch data for Dape. |
| `intellij.java.resolveClassDocument` | Resolves main-class metadata for run/debug. |
| `intellij.jvm.runMain` | CodeLens command routed into LSP-Proxy's Dape integration. |

Related Emacs options and commands:

| Name | Purpose |
| --- | --- |
| `lsp-proxy-java-file-templates` | Configure file templates for JVM source files. |
| `lsp-proxy-java-file-templates-on-create` | Enable or disable automatic template insertion for new files. |
| `M-x lsp-proxy-java-insert-file-template` | Insert a template manually. |
| `M-x lsp-proxy-java-open-import-log` | Open the current workspace import log. |

## jdtls Backend

### Switch Java to jdtls

Copy the built-in `[[language]]` block for Java into
`${user-emacs-directory}/lsp-proxy/languages.toml`, change its
`language-servers` value to `["jdtls"]`, then run `M-x lsp-proxy-restart`.

`language-servers` replaces the built-in list; it does not merge with it. In
normal use, list either `["intellij"]` or `["jdtls"]`, not both.

Kotlin has its own `[[language]]` block and stays on the IntelliJ backend.
jdtls does not serve Kotlin files.

### Install

Use the managed installer:

```elisp
M-x lsp-proxy-java-install-server
```

Then choose `jdtls` and restart LSP-Proxy. The managed install puts the server's
`bin/` directory on the proxy process `PATH`, so `command = "jdtls"` is enough.

| Setting/requirement | Purpose |
| --- | --- |
| `lsp-proxy-java-jdtls-version` | jdtls milestone to install. `nil` installs the latest known milestone. |
| `python3` | Required because `bin/jdtls` is a Python launcher. |
| JDK 21+ | Required to launch jdtls. It chooses `JAVA_HOME` first, then `java` on `PATH`. |

You can also install jdtls yourself, for example with a package manager, as long
as the `jdtls` launcher is on `PATH`.

### Configuration Fields

| Field | Default/example | Purpose |
| --- | --- | --- |
| `command` | `"jdtls"` | Starts the official launcher. Avoid hand-built `java -jar ... -data ...` commands because the launcher derives per-project workspace data. |
| `lombok-agent-arg` | `"--jvm-arg=-javaagent:{jar}"` | Tells LSP-Proxy how to append the managed Lombok agent when installed. |
| `[language-server.jdtls.config]` | table | Sent as jdtls `initializationOptions`. |
| `settings.java.referencesCodeLens.enabled` | `true` | Enables reference CodeLens. |
| `settings.java.implementationCodeLens` | `"all"` | Enables implementation CodeLens. Supported values: `"none"`, `"types"`, `"methods"`, `"all"`. |
| `settings.java.configuration.runtimes` | commented | Optional project compile JDK list. Controls project runtime model, not the JDK that launches jdtls. |

Example explicit runtimes:

```toml
[[language-server.jdtls.config.settings.java.configuration.runtimes]]
name = "JavaSE-1.8"
path = "/Library/Java/JavaVirtualMachines/zulu-8.jdk/Contents/Home"
default = true
```

When `runtimes` is absent, LSP-Proxy tries to infer a project JDK from common
project files such as `.java-version`, `.tool-versions`, `.sdkmanrc`, Maven
compiler settings, and Gradle toolchain/source compatibility. Use
`M-x lsp-proxy-java-select-project-jdk` for a session-local override; with a
prefix argument it clears the override and returns to automatic detection.

### Automatically Managed Pieces

| Piece | How to use |
| --- | --- |
| Java Debug bundle | Run `M-x lsp-proxy-java-install-debug`, or install jdtls through `M-x lsp-proxy-java-install-server`. The proxy injects the managed bundle at startup. |
| Lombok agent | Run `M-x lsp-proxy-java-install-lombok`, or install jdtls through the managed installer. The proxy appends the agent only when a managed jar exists. |
| Project runtime | Automatic unless explicit `settings.java.configuration.runtimes` entries are configured. |

If you maintain your own Lombok agent, put a `-javaagent:` argument containing
`lombok` in `args`; that suppresses the managed agent.

### Supported Capabilities and Methods

| Method/capability | What LSP-Proxy does |
| --- | --- |
| `classFileContentsSupport` | Enables dependency/JDK navigation. jdtls returns `jdt://` URIs and LSP-Proxy fetches source with `java/classFileContents`. |
| `progressReportProvider` | Enables jdtls `language/progressReport`; the proxy translates it onto normal LSP progress. |
| `advancedOrganizeImportsSupport` | Enables organize-imports code actions that require a client-side prompt. |
| `advancedGenerateAccessorsSupport` | Enables accessor-generation code actions that require a client-side prompt. |
| `overrideMethodsPromptSupport` | Enables override-methods code actions that require a client-side prompt. |
| `resolveAdditionalTextEditsSupport` | Lets completion resolve add imports through extra text edits. |
| `java.show.references` | Client-side CodeLens command; LSP-Proxy displays the locations carried by the resolved lens. |
| `java.show.implementations` | Client-side CodeLens command; LSP-Proxy displays the locations carried by the resolved lens. |
| `java/projectConfigurationUpdate` | Notification sent when LSP-Proxy needs jdtls to refresh project configuration. |

### Unsupported Extended Capabilities

Do not enable these jdtls extended capabilities until matching client handlers are
implemented:

| Capability | Status |
| --- | --- |
| `hashCodeEqualsPromptSupport` | Not implemented in the client. |
| `generateConstructorsPromptSupport` | Not implemented in the client. |
| `generateToStringPromptSupport` | Not implemented in the client. |
| `advancedExtractRefactoringSupport` | Not implemented in the client. |
| `moveRefactoringSupport` | Not implemented in the client. |

The corresponding code actions can still appear; without these extended
capability flags they run through the server-side fallback path instead of
requiring an unimplemented client prompt.
