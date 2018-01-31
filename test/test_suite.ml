let _ =
  Alcotest.run "Ogl_gui test suite" [
    "stylesheet", Test_stylesheet.test_suite;
  ]
