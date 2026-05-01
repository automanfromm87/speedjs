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
  Test_stability.run ();
  print_endline "\nAll tests passed.\n"
