# Serena MCP Configuration for Remote SSH Project

## Архитектура
- **Локальная машина**: VS Code + MCP-сервер Serena
- **Удалённая машина**: `rdev@f2d2819ccf9a` (172.19.0.2)
- **Проект**: `/home/rdev/ksformat` (R package)
- **Подключение**: SSH (192.168.31.136:49684 → 172.19.0.2:22)

## Созданные конфигурационные файлы

### На удалённой машине:
1. **`.serena/config.json`** - Конфигурация проекта с remote параметрами
2. **`.serenaproject`** - Маркер проекта
3. **`~/.serena/config.yaml`** - Глобальная конфигурация с VS Code Remote URI
4. **`~/.config/serena/projects.json`** - Альтернативная конфигурация
5. **`.vscode/settings.json`** - Настройки VS Code для MCP

## Текущий статус
❌ Serena MCP не видит зарегистрированных проектов  
✅ Конфигурация обновлена для работы через SSH  
✅ Используется схема `vscode-remote://ssh-remote+<host>/path`

## Для настройки локального MCP-сервера

### На ЛОКАЛЬНОЙ машине (где VS Code)

Создайте или обновите файл конфигурации MCP-сервера:

**Windows**: `%APPDATA%\Code\User\globalStorage\github.copilot-chat\mcpServers.json`  
**macOS**: `~/Library/Application Support/Code/User/globalStorage/github.copilot-chat/mcpServers.json`  
**Linux**: `~/.config/Code/User/globalStorage/github.copilot-chat/mcpServers.json`

```json
{
  "mcpServers": {
    "serena": {
      "command": "npx",
      "args": ["-y", "@oraios/serena-mcp"],
      "env": {
        "SERENA_CONFIG": "/path/to/serena-config.json"
      }
    }
  }
}
```

### Файл конфигурации Serena на локальной машине

Создайте файл `serena-config.json` на локальной машине:

```json
{
  "version": "1.0",
  "projects": {
    "ksformat": {
      "name": "ksformat",
      "path": "vscode-remote://ssh-remote+f2d2819ccf9a/home/rdev/ksformat",
      "type": "r-package",
      "language": "r",
      "description": "SAS-Style PROC FORMAT for R",
      "remote": {
        "type": "ssh",
        "host": "f2d2819ccf9a",
        "user": "rdev",
        "port": 22
      },
      "source_dirs": ["R"],
      "test_dirs": ["tests"]
    }
  }
}
```

### Альтернатива: VS Code settings.json

На локальной машине в User Settings (`Ctrl+,` → JSON):

```json
{
  "mcp.servers": {
    "serena": {
      "projects": {
        "ksformat": {
          "uri": "vscode-remote://ssh-remote+f2d2819ccf9a/home/rdev/ksformat",
          "type": "r-package"
        }
      }
    }
  }
}
```

## После настройки

1. **Перезапустить VS Code** полностью
2. **Переподключиться к SSH** (`Remote-SSH: Connect to Host...`)
3. **Проверить MCP сервер**: Открыть GitHub Copilot Chat и попробовать команду:
   ```
   @serena list projects
   ```

## Проверка подключения

```bash
# На удалённой машине
echo $SSH_CONNECTION
# Должно показать: 192.168.31.136 49684 172.19.0.2 22

hostname
# Должно показать: f2d2819ccf9a
```

## Если не работает

### Вариант 1: Прямой доступ без Serena
Используйте стандартные инструменты Copilot:
- `@workspace` для поиска в коде
- Прямые запросы без упоминания Serena

### Вариант 2: Локальная копия
Клонируйте проект на локальную машину и зарегистрируйте локальный путь в Serena

### Вариант 3: SSH туннель
Настройте SSH туннель для прямого доступа MCP-сервера к файлам

## Полезные команды

```bash
# Проверить SSH подключение
ssh rdev@f2d2819ccf9a 'ls -la /home/rdev/ksformat'

# Синхронизировать файлы конфигурации
scp ~/.serena/config.yaml rdev@f2d2819ccf9a:~/.serena/
```

## Контакты и документация
- Serena MCP: https://github.com/oraios/serena-mcp
- VS Code Remote SSH: https://code.visualstudio.com/docs/remote/ssh
