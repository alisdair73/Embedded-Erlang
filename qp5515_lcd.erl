-module(qp5515_lcd).
-include("qp5515.hrl").

-export([initialise/0,displayTextOnDisplay/1,resetDisplay/0]).

initialise() ->
  ListNames = [rs,enable,d0,d1,d2,d3,d4,d5,d6,d7],
  ListPins = [0,1,4,14,15,17,18,21,22,23],
  lists:foreach( fun({Name, Pin}) -> gpio_pin:setup_pin(Name,Pin) end, lists:zip(ListNames, ListPins) ),

  send_instruction(#instruction{name="Function Set",    register=command, d7=0,d6=0,d5=1,d4=1,d3=1,d2=0,d1=0,d0=0}),
  send_instruction(#instruction{name="Display Control", register=command, d7=0,d6=0,d5=0,d4=0,d3=1,d2=1,d1=1,d0=1}),
  send_instruction(#instruction{name="Entry Mode",      register=command, d7=0,d6=0,d5=0,d4=0,d3=0,d2=1,d1=1,d0=0}).


displayTextOnDisplay(TextToDisplay) -> 
  lists:foreach( fun(Instruction) -> send_instruction(Instruction) end, 
  	                   displayTextOnDisplay_helper(lists:reverse(TextToDisplay),[])).

displayTextOnDisplay_helper([],Instructions) -> Instructions;

displayTextOnDisplay_helper([H|T],Instructions) -> 
  displayTextOnDisplay_helper(T,[characterToInstruction([H])|Instructions]).

characterToInstruction(Character) ->
  <<D7:1,D6:1,D5:1,D4:1,D3:1,D2:1,D1:1,D0:1>> = list_to_binary(Character),
  #instruction{name=Character,register=data, d7=D7,d6=D6,d5=D5,d4=D4,d3=D3,d2=D2,d1=D1,d0=D0}.

send_instruction(Instruction) ->

  case Instruction#instruction.register of
    command  -> rs!{setbit,0};
    data     -> rs!{setbit,1}
  end,

  d7!{setbit, Instruction#instruction.d7},
  d6!{setbit, Instruction#instruction.d6},
  d5!{setbit, Instruction#instruction.d5},
  d4!{setbit, Instruction#instruction.d4},
  d3!{setbit, Instruction#instruction.d3},
  d2!{setbit, Instruction#instruction.d2},
  d1!{setbit, Instruction#instruction.d1}, 
  d0!{setbit, Instruction#instruction.d0},

  enable!{setbit,1},
  timer:sleep(100),
  enable!{setbit,0},
  timer:sleep(100).

 resetDisplay() ->
  send_instruction(#instruction{name="Reset Display",    register=command, d7=0,d6=0,d5=0,d4=0,d3=0,d2=0,d1=0,d0=1}).
