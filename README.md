vhdl-hydraulic
==============

An set of functions extending emacs vhdl-mode.el as an alternative to electric mode with state machine editing.

Interactive Functions:-

  vh-procedure        Insert a procedure template
  vh-function         Insert a function template
  vh-process-async    Insert an asynchronous process template
  vh-process-sync     Insert an asynchronous process template
  vh-process-del      Delete a process
  vh-sm               Insert a state machine template 
  vh-sm-add           Adds states to a state machine template
  vh-sm-del           Deletes states from a state machine template, or the whole template if none specified
  

Known bugs and limitations:-

Only upper case keywords are currently supported
CASE statements within state output or transtion clauses will screw up vh-sm-add and vh-sm-del

Why?

Electric mode is too interactive for my liking.  Most of the time I use the same clock and reset with the same sense in most of the processes in a design,  I probably don't know what all the inputs or outputs are yet, and want to fill in the comments later; getting prompted for all this every time interrupts my chain of thought.  The process templates here only prompt for the label when called interactively; the clock and reset are generated either from the last synchronous process in the buffer or the vhdl-mode default settings. State machine entry requires too much copy/pasting for my liking so I wanted to automate it.  This also required the ability to insert process templates with no user interaction.  Adding and deleting states also involves more editing than it has to so there are functions for this as well.
