---
name: ocaml-effects
description: |
  OCaml 5 algebraic effects + Effect.Deep handler patterns.
  Use when writing or debugging code that performs effects, captures
  continuations, or installs try_with handlers.
---
# OCaml 5 Algebraic Effects — practical patterns

## Effect declaration

Effects are typed via GADT extension on `Effect.t`:

```ocaml
type _ Effect.t +=
  | Get_user : int -> string Effect.t
  | Save_log : string -> unit Effect.t
```

The trailing type after `->` is the **return type** of the effect — what
`perform` evaluates to when the handler calls `continue k value`.

## Performing

```ocaml
let username = Effect.perform (Get_user 42) in
Effect.perform (Save_log ("loaded " ^ username))
```

`perform` blocks until the closest enclosing handler resolves the effect.

## Handling — Deep API

```ocaml
open Effect.Deep

let with_db f =
  try_with f () {
    effc = fun (type a) (eff : a Effect.t) ->
      match eff with
      | Get_user id ->
          Some (fun (k : (a, _) continuation) ->
            let name = Db.lookup id in
            continue k name)
      | Save_log msg ->
          Some (fun (k : (a, _) continuation) ->
            print_endline msg;
            continue k ())
      | _ -> None  (* Forward unmatched effects to outer handlers *)
  }
```

`Some` catches the effect; `None` re-raises it for the next outer handler.

## CRITICAL: raise vs discontinue

If the handler callback `(fun k -> ...)` raises an exception with `raise E`,
the exception escapes the **handler's `try_with`** — NOT the perform site.
This means:

```ocaml
try Effect.perform (Get_user id)   (* This try is USELESS *)
with Failure _ -> ...
```

won't catch a `Failure` raised inside the handler.

To raise AT the perform site (so a try/with at the perform site catches it),
use `discontinue`:

```ocaml
| Get_user _ -> Some (fun k -> discontinue k Not_found)
```

This is the only way to make retry-style handlers work — the inner retry
loop's try/with around `perform` only catches if the handler discontinues.

## Composition: re-perform from inside a handler

To layer handlers (observe + forward), perform the same effect inside your
callback:

```ocaml
| Get_user id ->
    Some (fun k ->
      log "fetching user %d" id;
      let result = Effect.perform (Get_user id) in   (* Goes to OUTER handler *)
      continue k result)
```

The re-perform escapes your own try_with (because callback runs OUTSIDE its
body) and reaches the next outer handler. This is how middleware-style
chains work.

## Effects don't cross threads

`Thread.create` and `Domain.spawn` start fresh effect stacks. Code that
performs an effect on a worker thread will get `Effect.Unhandled` because
the handler was on the parent thread.

Workarounds:
- Use `Eio.Fiber.fork` (Eio's scheduler is effect-aware)
- Or do the perform on the main thread + dispatch blocking work to a
  thread pool with channels

## When tools/handlers need to host more handlers

If a tool (like `delegate`) calls `Agent.run` internally, that nested run
performs effects. They must find a handler. Solution: the host handler
**re-installs itself** inside its Tool_call callback:

```ocaml
let production f =
  let rec wrap : type r. (unit -> r) -> r = fun thunk ->
    try_with thunk () { effc = ... 
      | Tool_call (n, i) ->
        Some (fun k ->
          let r = wrap (fun () -> run_tool n i) in   (* Recursive self-install *)
          continue k r)
      ...
    }
  in wrap f
```

Without this, sub-agent's effects propagate outside production's try_with
and end up Unhandled.
