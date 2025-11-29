# One Hundred Checkboxes - Sistema de Checkboxes Compartilhados

Sistema similar ao One Million Checkboxes, mas com 1000 checkboxes compartilhados entre todos os usuários.

## Pré-requisitos

### 1. Instalar GHC e Cabal

**Windows:**
- Baixe e instale o [GHCup](https://www.haskell.org/ghcup/)
- Execute no PowerShell:
```powershell
ghcup install ghc 9.6.7
ghcup install cabal
ghcup set ghc 9.6.7
```

**Linux (Ubuntu/Debian) / WSL:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
ghcup install ghc 9.6.7
ghcup install cabal
ghcup set ghc 9.6.7
```

**macOS:**
```bash
brew install ghc cabal-install
```

### 2. Instalar PostgreSQL

**Windows:**
- Baixe e instale o [PostgreSQL](https://www.postgresql.org/download/windows/)
- Durante a instalação, anote a senha do usuário `postgres`

**Linux (Ubuntu/Debian):**
```bash
sudo apt update
sudo apt install postgresql postgresql-contrib libpq-dev
```

**macOS:**
```bash
brew install postgresql@15
brew services start postgresql@15
```

### 3. Instalar bibliotecas de desenvolvimento do PostgreSQL

**Windows:**
- As bibliotecas já vêm com a instalação do PostgreSQL
- Configure as variáveis de ambiente antes de compilar (substitua `17` pela sua versão):
```powershell
$env:PATH += ";C:\Program Files\PostgreSQL\17\bin"
$env:LIB = "C:\Program Files\PostgreSQL\17\lib;$env:LIB"
$env:INCLUDE = "C:\Program Files\PostgreSQL\17\include;$env:INCLUDE"
```

**Linux:**
```bash
sudo apt install libpq-dev
```

**macOS:**
- Já incluído com Homebrew

## Configuração do Banco de Dados

### 1. Criar o banco de dados e usuário

Abra o terminal/psql e execute:

```sql
-- Conectar como superusuário
psql -U postgres

-- Criar banco de dados
CREATE DATABASE haskservant_db;

-- Criar usuário
CREATE USER haskservant WITH PASSWORD 'haskservant123';

-- Dar permissões
GRANT ALL PRIVILEGES ON DATABASE haskservant_db TO haskservant;

-- Conectar ao banco
\c haskservant_db

-- Dar permissões no schema
GRANT ALL ON SCHEMA public TO haskservant;
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO haskservant;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO haskservant;

-- Sair
\q
```

### 2. Executar a migração

```bash
psql -U haskservant -d haskservant_db -f migration.sql
```

## Compilação e Execução

### 1. Atualizar o índice do Cabal

```bash
cabal update
```

### 2. Compilar o projeto

```bash
cabal build
```

### 3. Executar o servidor

**Opção 1: Usando cabal run**
```bash
cabal run one-hundred-checkboxes
```

**Opção 2: Executar o binário diretamente**
```bash
cabal install --installdir=.
./one-hundred-checkboxes
```

**Opção 3: Definir variáveis de ambiente (se necessário)**
```bash
# Windows PowerShell
$env:DB_HOST="localhost"
$env:DB_PORT="5432"
$env:DB_NAME="haskservant_db"
$env:DB_USER="haskservant"
$env:DB_PASSWORD="haskservant123"
cabal run one-hundred-checkboxes

# Linux/macOS
export DB_HOST=localhost
export DB_PORT=5432
export DB_NAME=haskservant_db
export DB_USER=haskservant
export DB_PASSWORD=haskservant123
cabal run one-hundred-checkboxes
```

### 4. Acessar o frontend

Abra o arquivo `frontend/checkboxes.html` no navegador ou use um servidor HTTP simples:

```bash
# Python 3
cd frontend
python -m http.server 3000

# Node.js (com http-server)
npx http-server frontend -p 3000
```

Depois acesse: `http://localhost:3000/checkboxes.html`

**Nota:** Se usar um servidor HTTP diferente da porta 8080, atualize a constante `API_URL` no arquivo `frontend/checkboxes.js`.

## Estrutura do Projeto

```
one-hundred-checkboxes/
├── app/
│   ├── Main.hs              # Ponto de entrada da aplicação
│   ├── Server/
│   │   └── Routes.hs        # Definição das rotas da API
│   └── Api/
│       └── Model.hs         # Modelos de dados
├── frontend/
│   ├── checkboxes.html      # Interface do usuário
│   └── checkboxes.js        # Lógica do frontend
├── migration.sql            # Script de criação do banco
├── one-hundred-checkboxes.cabal  # Configuração do projeto
└── cabal.project            # Configuração do Cabal
```

## API Endpoints

- `GET /health` - Healthcheck
- `GET /checkboxes` - Retorna todos os checkboxes
- `PATCH /checkboxes/:id` - Atualiza um checkbox específico
- `OPTIONS /checkboxes` - CORS preflight

## Solução de Problemas

### Erro de conexão com PostgreSQL

Verifique se o PostgreSQL está rodando:
```bash
# Windows
# Verifique no Services (services.msc) se o serviço PostgreSQL está rodando

# Linux
sudo systemctl status postgresql

# macOS
brew services list
```

### Erro de permissão no banco

Certifique-se de que o usuário tem as permissões corretas:
```sql
GRANT ALL PRIVILEGES ON ALL TABLES IN SCHEMA public TO haskservant;
GRANT ALL PRIVILEGES ON ALL SEQUENCES IN SCHEMA public TO haskservant;
```

### Erro ao compilar - "Library requirements (PostgreSQL) not met" (Windows)

Configure as variáveis de ambiente antes de compilar (substitua `17` pela sua versão):
```powershell
$env:PATH += ";C:\Program Files\PostgreSQL\17\bin"
$env:LIB = "C:\Program Files\PostgreSQL\17\lib;$env:LIB"
$env:INCLUDE = "C:\Program Files\PostgreSQL\17\include;$env:INCLUDE"
cabal clean
cabal build
```

**Nota:** As variáveis de ambiente são válidas apenas na sessão atual do PowerShell. Se fechar e abrir uma nova janela, configure novamente antes de compilar.

### Erro ao compilar (Linux/macOS)

Certifique-se de ter todas as dependências do sistema:
- `libpq-dev` (Linux)
- PostgreSQL instalado com bibliotecas de desenvolvimento
