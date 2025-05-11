import std/[random]
import flashcard, noun, verb

randomize()
let nouns = fileToNouns("nouns.tsv", 0..23)
let verbs = fileToVerbs("verbs.tsv", 0..23)

#let verbs = fileToVerbs("verbs.tsv", 0..19)
var score: Score
while true:
  var fc = case rand(0..1):
    of 0: flashcard(sample(nouns))
    else: flashcard(sample(verbs))
  play(fc, score)