(** Test driver. Each [Test_*] module exposes a [run ()] entry point
    that executes its tests in registration order. *)

let () =
  Test_step.run ();
  Test_agent.run ();
  Test_planner.run ();
  Test_skill.run ();
  Test_conversation.run ();
  Test_context.run ();
  Test_llm_handler.run ();
  Test_tool_handler.run ();
  Test_governor.run ();
  Test_effects.run ();
  Test_e2e.run ();
  Test_parallel.run ();
  Test_agent_algebra.run ();
  Test_cache_stability.run ();
  Test_stability.run ();
  Test_chaos.run ();
  print_endline "\nAll tests passed.\n"
