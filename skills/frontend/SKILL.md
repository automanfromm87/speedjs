---
name: frontend
description: |
  Opinionated frontend stack: Vite + React 18 + TypeScript + Tailwind v3 + Lucide.
  Load this skill BEFORE creating any frontend project. Covers full setup,
  directory layout, TypeScript patterns, styling conventions, and the gotchas
  that bite you on first build (verbatimModuleSyntax, content paths, etc).
---
# Opinionated Frontend Stack — Vite + React 18 + TypeScript + Tailwind v3 + Lucide

This is the ONLY frontend stack we use. No alternatives — don't suggest Next.js,
shadcn-ui generators, CRA, daisyUI, MUI, etc. unless explicitly asked.

---

## 1. Setup checklist (5 commands)

```bash
# Scaffold (non-interactive)
cd /tmp && npm create vite@latest my-app -- --template react-ts

# Install base deps
cd my-app && npm install

# Tailwind v3 — PIN the major version
npm install -D tailwindcss@^3.4 postcss autoprefixer

# Both configs in one shot
npx tailwindcss init -p

# Lucide icons
npm install lucide-react
```

Total: 5 commands. **Don't `ls -la` between them — trust the exit code.**
**NEVER use Tailwind v4** — different setup, training data assumes v3.

## 2. Required edits after install

### `tailwind.config.js`

```js
/** @type {import('tailwindcss').Config} */
export default {
  content: ['./index.html', './src/**/*.{ts,tsx}'],
  theme: { extend: {} },
  plugins: [],
}
```

### `src/index.css`

REPLACE entire file with these 3 lines only:

```css
@tailwind base;
@tailwind components;
@tailwind utilities;
```

### Files to DELETE / REPLACE

- `src/App.css` → DELETE (or make empty)
- `src/App.tsx` → REPLACE entirely (Vite default has demo content)
- `public/vite.svg`, `src/assets/*` → keep, ignore unless used

## 3. CRITICAL: `verbatimModuleSyntax`

Vite's default `tsconfig.json` enables `verbatimModuleSyntax: true`.
**Type-only imports MUST use `import type`** or build fails with TS1484.

```tsx
// ❌ FAILS
import { KeyboardEvent } from 'react';
import { Filter } from '../types';

// ✅ Correct
import { type KeyboardEvent, useState } from 'react';
import type { Filter } from '../types';
```

Common type-only React imports: `KeyboardEvent`, `MouseEvent`, `ChangeEvent`,
`FormEvent`, `ReactNode`, `FC`, `Dispatch`, `SetStateAction`.

## 4. Directory layout (small-to-medium app)

```
src/
├── main.tsx              ← Vite entry; renders <App />
├── App.tsx               ← Top-level layout
├── index.css             ← Tailwind directives ONLY
├── types.ts              ← Shared types/interfaces
│
├── components/           ← Reusable UI components (PascalCase.tsx)
├── hooks/                ← Custom hooks (useFoo.ts, camelCase, use-prefix)
├── lib/                  ← Pure utilities (no React) — formatDate.ts, api.ts
└── pages/                ← Route-level components (only if using a router)
```

**File naming:**
- Component: `PascalCase.tsx` (e.g. `TodoItem.tsx`)
- Hook: `useFoo.ts` (must start with `use`)
- Utility: `camelCase.ts`
- One default export per file

**Promote to a separate file when:**
- Component is reused across files
- Component is > 30 lines
- It's a route page

## 5. Component template

```tsx
import { type KeyboardEvent, useState } from 'react';
import { Trash2 } from 'lucide-react';
import type { Todo } from '../types';

interface TodoItemProps {
  todo: Todo;
  onToggle: (id: string) => void;
  onDelete: (id: string) => void;
}

export default function TodoItem({ todo, onToggle, onDelete }: TodoItemProps) {
  const handleKeyDown = (e: KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') onToggle(todo.id);
  };

  return (
    <li className="group flex items-center gap-2 p-2 hover:bg-gray-50">
      <input
        type="checkbox"
        checked={todo.completed}
        onChange={() => onToggle(todo.id)}
      />
      <span className={todo.completed ? 'line-through text-gray-400' : ''}>
        {todo.text}
      </span>
      <button
        aria-label="Delete"
        onClick={() => onDelete(todo.id)}
        className="ml-auto opacity-0 group-hover:opacity-100 transition text-red-600"
      >
        <Trash2 className="w-4 h-4" />
      </button>
    </li>
  );
}
```

Key conventions visible:
- Props interface above component, named `<Component>Props`
- Default export
- `import type` for types-only imports
- Tailwind utility classes ONLY — no `style={{}}` no `.css` files
- `className`, not `class`
- No `: JSX.Element` annotation — let TS infer
- No `import React` — JSX transform handles it

## 6. State management

**Default: React's built-in `useState` + `useEffect`**.

For shared state across many components: **Zustand** (`npm install zustand`).
Don't reach for Redux unless you have a 50+ component app.

**Custom hook over Context** for reusable stateful logic:

```tsx
// hooks/useTodos.ts
import { useState, useEffect } from 'react';
import type { Todo } from '../types';

const STORAGE_KEY = 'speedjs-todos';

export function useTodos() {
  const [todos, setTodos] = useState<Todo[]>(() => {
    const raw = localStorage.getItem(STORAGE_KEY);
    return raw ? JSON.parse(raw) : [];
  });

  useEffect(() => {
    localStorage.setItem(STORAGE_KEY, JSON.stringify(todos));
  }, [todos]);

  return {
    todos,
    addTodo: (text: string) =>
      setTodos((prev) => [
        ...prev,
        { id: crypto.randomUUID(), text, completed: false },
      ]),
    toggleTodo: (id: string) =>
      setTodos((prev) =>
        prev.map((t) => (t.id === id ? { ...t, completed: !t.completed } : t))
      ),
    deleteTodo: (id: string) =>
      setTodos((prev) => prev.filter((t) => t.id !== id)),
  };
}
```

**Always**:
- Object return (not array) for custom hooks — named, readable at call site
- `as const` only for `useState`-mimicking 2-tuple hooks
- `id: crypto.randomUUID()` for client-generated IDs

## 7. Styling rules — Tailwind only

```tsx
// ✅ Right
<div className="max-w-2xl mx-auto bg-white rounded-xl shadow-md p-6">

// ❌ Wrong
<div style={{ maxWidth: '672px', margin: '0 auto' }}>
<div className={css.container}>
```

**Standard utility combos** (memorize):

| Goal | Classes |
|---|---|
| Centered card | `max-w-2xl mx-auto` |
| Modern card | `bg-white rounded-xl shadow-md p-6` |
| Soft background | `bg-gradient-to-br from-blue-50 to-indigo-100 min-h-screen` |
| Hover lift | `transition-shadow hover:shadow-lg` |
| Text input | `w-full px-4 py-2 border border-gray-300 rounded-lg focus:ring-2 focus:ring-blue-500 focus:border-transparent transition` |
| Primary button | `px-4 py-2 bg-blue-600 text-white rounded-lg hover:bg-blue-700 active:bg-blue-800 transition disabled:bg-gray-300` |
| Secondary button | `px-4 py-2 border border-gray-300 text-gray-700 rounded-lg hover:bg-gray-50 transition` |
| Danger button | `px-3 py-1 text-sm text-red-600 hover:bg-red-50 rounded transition` |
| Empty state | `flex flex-col items-center py-16 text-gray-500` |
| Strikethrough done | `line-through text-gray-400` |
| Hover-reveal action | `group` on parent + `opacity-0 group-hover:opacity-100 transition` on action |

**Don't write arbitrary values** when a class exists:
- `text-sm` not `text-[14px]`
- `gap-2` not `gap-[8px]`

## 8. Lucide icons

```tsx
import { Plus, Trash2, Check, X, Loader2 } from 'lucide-react';

<Plus className="w-4 h-4" />        // size via Tailwind, not size prop
<Trash2 className="w-4 h-4 text-red-600" />  // color via text-* class
<Loader2 className="w-4 h-4 animate-spin" /> // spinner pattern
```

**Common picks:**
| Action | Icon |
|---|---|
| Add | `Plus` |
| Delete | `Trash2` (NOT `Trash`) |
| Edit | `Pencil` |
| Save / Confirm | `Check` |
| Cancel / Close | `X` |
| Loading | `Loader2` + `animate-spin` |
| Search | `Search` |
| Settings | `Settings` |
| Empty (todo) | `ClipboardList` |
| Empty (generic) | `Inbox` |
| Warning | `AlertTriangle` |
| Success | `CheckCircle2` |

**Sizes**: `w-4 h-4` (16px button), `w-5 h-5` (20px default), `w-12 h-12+` (empty state).

**Accessibility**: `aria-hidden` for decorative; `aria-label` on the parent button if icon is the only content.

## 9. Verification

```bash
npm run build
```

Expected success output:
```
✓ NN modules transformed.
dist/index.html             0.45 kB
dist/assets/index-XXX.css   ~9 kB
dist/assets/index-XXX.js  ~200 kB
✓ built in NNNms
```

If TS1484 → fix `import type`. If "Cannot find module" → forgot `npm install`.

**NEVER run `npm run dev`** during agent runs — interactive, hangs forever.
Use `npm run build` to verify type correctness.

## 10. Final don'ts

- Don't `import React from 'react'` (React 17+ JSX transform)
- Don't `: JSX.Element` annotation
- Don't `React.FC<Props>` — plain function declarations
- Don't `any` — use `unknown` and narrow
- Don't separate `.css` / `.module.css` files (except `index.css` for Tailwind)
- Don't `class="..."` (use `className`)
- Don't `defaultProps` (destructure with defaults)
- Don't put `'use client'` (that's Next.js, we're using Vite)
- Don't generate routes via shadcn-ui CLI / templates — write your own with React Router if needed
