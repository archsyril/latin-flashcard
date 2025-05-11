import std/[random, strutils, strformat, terminal]

type
  Flashcard* = ref object
    q*, a*, i*: string
  
  Score* = tuple
    good, bad: int

proc waitUntilInput(validChars: set[char]): char =
  result = getch()
  while result notin validChars:
    result = getch() # read any character a-z, 0-9

proc generateUniqueChars(validChars: static[set[char]]): tuple[a, b: char] =
  # generate two letters which cannot be the same
  result.a = sample(validChars)
  result.b  = sample(validChars)
  while result.b == result.a: # do not allow goodc and badc to be the same
    result.b = sample(validChars)


proc play*(fc: Flashcard, s: var Score) =
  const validChars = {'a'..'z', '0'..'9'}

  stdin.hideCursor()
  stdout.eraseScreen()
  
  echo fc.q
  discard waitUntilInput(validChars)

  let (good, bad) = generateUniqueChars(validChars)
  
  #stdout.eraseLine()
  stdout.writeLine("\n")
  
  echo fmt"{fc.a}{'\n'}   {fc.i}"
  echo fmt"Correct / Incorrect answer: {good} / {bad}"
  
  if good == waitUntilInput({good, bad}):
    inc s.good
  else:
    inc s.bad