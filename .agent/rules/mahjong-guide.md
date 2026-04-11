---
trigger: always_on
---

## 專案開發規則：麻將AI

### 1. 技術棧 (Tech Stack)
- 後端：使用 .NET 8 Web API + SQLite (Entity Framework Core)。遵循 Clean Architecture。 目錄名稱: backend
- 前端：使用 Vite + React (TypeScript) + Material UI (MUI)。 目錄名稱: mahjong-game
- Library：將麻將有關的AI演算法單獨寫成一個C# dll專案。目錄名稱: mahjong-thinking

### 2. 語言與文件規範
- 註解語言：程式碼中的「所有註解」必須使用 **繁體中文**。
- 變數命名：使用語義化的英文。
- UI 文字：所有介面顯示的文字一律使用 **繁體中文**。