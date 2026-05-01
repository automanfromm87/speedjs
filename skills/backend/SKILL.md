---
name: backend
description: |
  Opinionated backend stack: uv (package manager) + Python 3.12+ + FastAPI +
  Pydantic v2 + SQLAlchemy 2.0 (when DB needed). Load this BEFORE creating any
  backend project. Covers full setup, project layout, routing, dependency
  injection, async patterns, and the gotchas (pydantic v1 vs v2, sync vs async).
---
# Opinionated Backend Stack — uv + Python 3.12+ + FastAPI + Pydantic v2

This is the ONLY backend stack we use. No alternatives — don't suggest pip,
poetry, conda, Flask, Django, Sanic, etc. unless explicitly asked.

---

## 1. Why uv

`uv` is Astral's Rust-based Python package manager. **10-100x faster than pip**.
It replaces: `pip`, `pip-tools`, `pyenv`, `virtualenv`, `pipx`. One tool.

```bash
# Install once (system-wide):
curl -LsSf https://astral.sh/uv/install.sh | sh
```

## 2. Setup checklist (3 commands)

```bash
# Create project
uv init my-api && cd my-api

# Add deps
uv add fastapi 'uvicorn[standard]' pydantic-settings

# (optional) DB layer
uv add sqlalchemy alembic 'pydantic[email]'
```

That's it. uv handles venv, lockfile (`uv.lock`), and pyproject.toml.

**Don't** use `pip install` — once you `uv init` the project, all installs
go through `uv add`.

## 3. Project layout

```
my-api/
├── pyproject.toml            ← uv-managed, deps + project metadata
├── uv.lock                   ← lockfile (commit this!)
├── .python-version           ← uv-managed Python version pin
├── README.md
│
├── app/                      ← Python package (the actual app)
│   ├── __init__.py
│   ├── main.py               ← FastAPI app instantiation, route registration
│   ├── config.py             ← pydantic-settings BaseSettings (env vars)
│   │
│   ├── api/                  ← HTTP endpoints
│   │   ├── __init__.py
│   │   ├── todos.py          ← /todos endpoints (one router per resource)
│   │   └── users.py
│   │
│   ├── models/               ← Pydantic models (request/response shapes)
│   │   ├── __init__.py
│   │   └── todo.py
│   │
│   ├── db/                   ← Database (only if persistent storage)
│   │   ├── __init__.py
│   │   ├── session.py        ← SQLAlchemy async engine + session factory
│   │   └── models.py         ← SQLAlchemy ORM models
│   │
│   └── services/             ← Business logic (no HTTP, no DB SQL)
│       └── todo_service.py
│
└── tests/
    └── test_todos.py         ← pytest, async via pytest-asyncio
```

## 4. `app/main.py` template

```python
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from app.api import todos, users
from app.config import settings

app = FastAPI(
    title="My API",
    version="0.1.0",
)

# CORS — be explicit about allowed origins in prod
app.add_middleware(
    CORSMiddleware,
    allow_origins=settings.cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)

# Routers — one per resource
app.include_router(todos.router, prefix="/todos", tags=["todos"])
app.include_router(users.router, prefix="/users", tags=["users"])


@app.get("/health")
async def health():
    return {"status": "ok"}
```

## 5. `app/config.py` — pydantic-settings

```python
from pydantic import Field
from pydantic_settings import BaseSettings, SettingsConfigDict


class Settings(BaseSettings):
    """All env-driven config goes here.

    Precedence: env var > .env file > default.
    """
    model_config = SettingsConfigDict(env_file=".env", extra="ignore")

    # Required
    database_url: str = Field(default="sqlite+aiosqlite:///./app.db")

    # Optional with defaults
    cors_origins: list[str] = Field(default_factory=lambda: ["http://localhost:5173"])
    log_level: str = "INFO"


settings = Settings()
```

Use `from app.config import settings` everywhere — DON'T read `os.getenv` directly.

## 6. Route module template — `app/api/todos.py`

```python
from fastapi import APIRouter, HTTPException, status
from app.models.todo import TodoCreate, TodoResponse
from app.services import todo_service

router = APIRouter()


@router.get("", response_model=list[TodoResponse])
async def list_todos():
    return await todo_service.list_all()


@router.post("", response_model=TodoResponse, status_code=status.HTTP_201_CREATED)
async def create_todo(payload: TodoCreate):
    return await todo_service.create(payload)


@router.get("/{todo_id}", response_model=TodoResponse)
async def get_todo(todo_id: str):
    todo = await todo_service.get(todo_id)
    if todo is None:
        raise HTTPException(status_code=404, detail="Todo not found")
    return todo


@router.delete("/{todo_id}", status_code=status.HTTP_204_NO_CONTENT)
async def delete_todo(todo_id: str):
    deleted = await todo_service.delete(todo_id)
    if not deleted:
        raise HTTPException(status_code=404, detail="Todo not found")
```

**Conventions:**
- `router = APIRouter()` (no prefix here — set in `main.py` `include_router`)
- All handlers `async def`
- `response_model=` on every route (auto-validation + OpenAPI docs)
- `HTTPException` for 4xx/5xx — never return error JSON manually
- Path params typed (`todo_id: str`) — FastAPI auto-validates

## 7. Pydantic v2 models — `app/models/todo.py`

```python
from datetime import datetime
from pydantic import BaseModel, Field, ConfigDict


class TodoCreate(BaseModel):
    """Request body for creating a todo."""
    text: str = Field(min_length=1, max_length=500)


class TodoResponse(BaseModel):
    """API response shape — returned to clients."""
    model_config = ConfigDict(from_attributes=True)  # SQLAlchemy → Pydantic

    id: str
    text: str
    completed: bool
    created_at: datetime
```

**Pydantic v2 differences from v1** (don't mix patterns):
- `model_config = ConfigDict(...)` not `class Config:`
- `from_attributes=True` not `orm_mode=True`
- `model_dump()` not `dict()`
- `model_validate()` not `parse_obj()`
- `Field(default_factory=list)` for mutable defaults

## 8. Database — SQLAlchemy 2.0 async

`app/db/session.py`:

```python
from sqlalchemy.ext.asyncio import async_sessionmaker, create_async_engine
from app.config import settings

engine = create_async_engine(settings.database_url, echo=False)
SessionLocal = async_sessionmaker(engine, expire_on_commit=False)


async def get_db():
    """FastAPI dependency — yields a session per request."""
    async with SessionLocal() as session:
        yield session
```

`app/db/models.py`:

```python
from datetime import datetime
from uuid import uuid4
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column


class Base(DeclarativeBase):
    pass


class Todo(Base):
    __tablename__ = "todos"

    id: Mapped[str] = mapped_column(primary_key=True, default=lambda: str(uuid4()))
    text: Mapped[str]
    completed: Mapped[bool] = mapped_column(default=False)
    created_at: Mapped[datetime] = mapped_column(default=datetime.utcnow)
```

Use `Mapped[T]` + `mapped_column()` (the SQLAlchemy 2.0 typed style),
NOT `Column(...)` from 1.x.

Use as a FastAPI dependency:

```python
from fastapi import Depends
from sqlalchemy.ext.asyncio import AsyncSession
from app.db.session import get_db

@router.get("/{todo_id}")
async def get_todo(todo_id: str, db: AsyncSession = Depends(get_db)):
    result = await db.get(Todo, todo_id)
    if result is None:
        raise HTTPException(404)
    return result
```

## 9. Running

```bash
# Dev (auto-reload)
uv run fastapi dev app/main.py

# Or manual uvicorn (more control)
uv run uvicorn app.main:app --reload --port 8000

# Production-style
uv run uvicorn app.main:app --host 0.0.0.0 --port 8000
```

`fastapi dev` is the new (FastAPI 0.110+) shorthand. Visit `http://localhost:8000/docs`
for auto-generated Swagger UI.

**For agent verification (no interactive process)**:

```bash
# Just check imports + type-check, don't start server
uv run python -c "from app.main import app; print(app.title)"
```

## 10. Testing

`pyproject.toml` add dev dep:
```bash
uv add --dev pytest pytest-asyncio httpx
```

`tests/test_todos.py`:
```python
import pytest
from httpx import AsyncClient, ASGITransport
from app.main import app


@pytest.mark.asyncio
async def test_health():
    async with AsyncClient(transport=ASGITransport(app=app), base_url="http://test") as ac:
        r = await ac.get("/health")
        assert r.status_code == 200
        assert r.json() == {"status": "ok"}
```

Run: `uv run pytest`

## 11. Common pitfalls

| Pitfall | Fix |
|---|---|
| `from fastapi import FastAPI` import slow | Pre-warm with `uv pip install` (uv handles caching) |
| Sync `def` route blocks event loop | Always `async def`; use `asyncio.to_thread` for sync IO |
| Pydantic v1 `Config` class in v2 code | Migrate to `model_config = ConfigDict(...)` |
| Mutable default in Pydantic field | `Field(default_factory=list)` not `= []` |
| `os.getenv()` scattered | Centralize in `Settings` |
| ORM model returned directly to API | Add `model_config = ConfigDict(from_attributes=True)` to response model |
| `psycopg2` (sync) for async app | Use `asyncpg` for Postgres, `aiosqlite` for SQLite |
| `npm`-style scripts | Add `[tool.uv.scripts]` in pyproject or use Makefile |

## 12. What NOT to add unless asked

- Celery / Redis (overkill — use FastAPI background tasks first)
- Authentication frameworks (FastAPI's `Depends` + JWT is enough for most)
- ORM choice other than SQLAlchemy 2.0
- Sync FastAPI (always async)
- Multiple Python versions (uv pin one in `.python-version`)
- Docker (only when explicitly deploying)

## 13. Don'ts

- Don't `pip install` after `uv init` — use `uv add`
- Don't `import os; os.getenv("FOO")` — use `from app.config import settings`
- Don't `class Config:` style (Pydantic v1) — use `ConfigDict`
- Don't `def` routes — `async def`
- Don't return ORM models directly without `from_attributes=True`
- Don't mix sync (`requests`, `psycopg2`) with async — use `httpx`, `asyncpg`
- Don't put business logic in route handlers — put in `services/`
- Don't write `try: ... except: pass` — let FastAPI's error handlers / HTTPException do the work
