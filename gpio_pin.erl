-module(gpio_pin).
-export([setup_pin/2,loop/1]).

initialise(GPIOPinNo) ->
  unexport(GPIOPinNo),

  {ok, ExportRef} = file:open("/sys/class/gpio/export", [write]),
  file:write(ExportRef, integer_to_list(GPIOPinNo)),
  file:close(ExportRef),

  {ok, DirectionRef} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(GPIOPinNo) ++ "/direction", [write]),
  file:write(DirectionRef, "out"),
  file:close(DirectionRef),

  {ok, ValueRef} = file:open("/sys/class/gpio/gpio" ++ integer_to_list(GPIOPinNo) ++ "/value", [read, write]).

unexport(GPIOPinNo) ->
  {ok, UnexportRef} = file:open("/sys/class/gpio/unexport", [write]),
  file:write(UnexportRef, integer_to_list(GPIOPinNo)),
  file:close(UnexportRef).

setup_pin(PinName,GPIOPinNo) ->
  {ok, ValueRef} = initialise(GPIOPinNo),
  register(PinName,spawn(?MODULE, loop, [ValueRef])).

loop(ValueRef) ->
  receive
    {setbit,Value} -> file:write(ValueRef, integer_to_list(Value))
  end,
  loop(ValueRef).