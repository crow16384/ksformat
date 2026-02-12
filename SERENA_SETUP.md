# Настройка Serena MCP для удалённого проекта

## Быстрая инструкция

Проект работает через SSH: `rdev@f2d2819ccf9a:/home/rdev/ksformat`

### Что нужно сделать на ЛОКАЛЬНОЙ машине с VS Code:

1. **Найдите файл конфигурации MCP:**
   - Windows: `%APPDATA%\Code\User\globalStorage\github.copilot-chat\mcpServers.json`
   - macOS: `~/Library/Application Support/Code/User/globalStorage/github.copilot-chat/mcpServers.json`
   - Linux: `~/.config/Code/User/globalStorage/github.copilot-chat/mcpServers.json`

2. **Добавьте конфигурацию:**
   ```json
   {
     "mcpServers": {
       "serena": {
         "projects": {
           "ksformat": {
             "uri": "vscode-remote://ssh-remote+f2d2819ccf9a/home/rdev/ksformat",
             "type": "r-package",
             "language": "r"
           }
         }
       }
     }
   }
   ```

3. **Перезапустите VS Code**

4. **Проверьте:** В Copilot Chat напишите `@serena list projects`

### Подробности

См. [.serena/README.md](.serena/README.md) для полной документации по настройке.

---

**Статус проекта:** ✅ Пакет полностью функционален и готов к использованию  
**Проверка:** `R CMD check --no-manual ksformat_0.1.0.tar.gz` - **Status: OK**
