import std/[strutils, strformat, random]

# Last thing worked on: translations, move everything from study to interpreter
# Make ConjDesc a ref?

type
  Part* = enum PresActInd, Infinitive, PerfActInd, PerfPassPart
  Verb* = object
    conj: Conjugation
    parts: array[4, string]
    trans: seq[string]
    unit: uint8 # wherein 0 indicates a verb taken outside of the textbook
  Conjugated = array[6, string]
  SpecConj = tuple
    verb: string
    person: Person
    tran: string
  ConjDesc* = object
    name: string
    tran: Translation
    part: Part
    exceptions: seq[ExceptFor]
    stem: seq[string]
    forms: seq[ConjForm]
  ExceptFor = tuple
    conj: seq[Conjugation]
    person: Person
    rule: string
  ConjForm = tuple
    conj: seq[Conjugation]
    endings: array[6, string]
  Conjugation* = enum First, Second, Third_o,
                     Third_io, Fourth, Irregular
  Person* = enum FirstSing = "First Person Singular", SecondSing = "Second Person Singular", ThirdSing = "Third Person Singular",
                 FirstPlur = "First Person Plural", SecondPlur = "Second Person Plural", ThirdPlur = "Third Person Plural"

  Translation* = object
    form: string
    case twoAux: bool
    of true:
      aux: array[2, string]
    else: discard
    case moreOptions: bool
    of true:
      options: seq[string]
    else: discard

proc `[]`[T](a: openArray[T], i: Part or Conjugation or Person): T = a[i.int]
proc `[]=`[T](a: var openArray[T], i: Part or Conjugation or Person, v: T) = a[i.int] = v

# Lauda-, Mone-, Duce-, Capie-, Audie-... 
# Lauda, Mone, Duce, Capi, Audi 
const StemAddition = ["a", "e", "e", "ie", "ie"] 
#var conjDescs*: Table[string, ConjDesc]
const conjDescs* = [
  ########## Active ##########
  ConjDesc(
    name: "Present Active Indicative",
    tran: Translation(form: "$pronoun [$eng_verb]"),
    part: Infinitive,
    stem: @["a", "e", "i", "i", "i"],
    forms: @[ 
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["*o", "*s", "*t", "*mus", "*tis", "*nt"]
      )
    ],
    exceptions: @[
      (conj: @[First, Third_o], person: FirstSing, rule: "o"),
      (conj: @[Third_o], person: ThirdPlur, rule: "unt"),
      (conj: @[Third_io, Fourth], person: ThirdPlur, rule: "iunt")
    ]
  ),
  ConjDesc(
    name: "Imperfect Active Indicative",
    tran: Translation(
      form: "$pronoun $aux [$eng_verb]ing",
      twoAux: true, aux: ["was", "were"]
    ),
    part: Infinitive,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["*bam", "*bas", "*bat", "*bamus", "*batis", "*bant"]
      )
    ]
  ),
  ConjDesc(
    name: "Future Active Indicative",
    tran: Translation(form: "$pronoun will [$eng_verb]"),
    part: Infinitive,
    forms: @[
      (
        conj: @[First, Second],
        endings: ["*bo", "*bis", "*bit", "*bimus", "*bitis", "*bunt"]
      ),
      (
        conj: @[Third_o, Third_io, Fourth],
        endings: ["iam", "*s", "*t", "*mus", "*tis", "*nt"]
      )
    ],
    exceptions: @[
      ( conj: @[Third_o], person: FirstSing, rule: "am" )
    ]
  ),
  ConjDesc(
    name: "Perfect Active Indicative",
    tran: Translation(
      form: "$pronoun $option [$eng_verb]ed",
      moreOptions: true, options: @["", "have"]
    ),
    part: PerfActInd,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["i", "isti", "it", "imus", "istis", "erunt|ere"] # Verify 3p plur extra
      )
    ]
  ),
  ConjDesc(
    name: "Pluperfect Active Indicative",
    tran: Translation(form: "$pronoun had [$eng_verb]ed"),
    part: PerfActInd,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["eram", "eras", "erat", "eramus", "eratis", "erant"]
      )
    ]
  ),
  ConjDesc(
    name: "Future-Perfect Active Indicative",
    tran: Translation(form: "$pronoun will have [$eng_verb]ed"),
    part: PerfActInd,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["ero", "eris", "erit", "erimus", "eritis", "erint"]
      )
    ]
  ),
  ConjDesc(
    name: "Present Active Periphrastic",
    tran: Translation(
      form: "$pronoun $aux $option to [$eng_verb]",
      twoAux: true, aux: ["am", "are"],
      moreOptions: true, options: @["about", "going", "intending"]
    ),
    part: PerfPassPart,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["urus/a/um sum", "urus/a/um es", "urus/a/um est",
                  "uri/ae/a sumus", "uri/ae/a estis", "uri/ae/a sunt"]
      )
    ]
  ),
  ConjDesc(
    name: "Imperfect Active Periphrastic",
    tran: Translation(
      form: "$pronoun $aux $option to [$eng_verb]",
      twoAux: true, aux: ["was", "were"],
      moreOptions: true, options: @["about", "going", "intending"]
    ),
    part: PerfPassPart,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["urus/a/um eram", "urus/a/um eras", "urus/a/um erat",
                  "uri/ae/a eramus", "uri/ae/a eratis", "uri/ae/a erant"]
      )
    ]
  ),
  ConjDesc(
    name: "Pluperfect Active Periphrastic",
    tran: Translation(
      form: "$pronoun had been $option to [$eng_verb]",
      moreOptions: true, options: @["about", "intending"]
    ),
    part: PerfPassPart,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["urus/a/um fueram", "urus/a/um fueras", "urus/a/um fuerat",
                  "uri/ae/a fueramus", "uri/ae/a fueratis", "uri/ae/a fuerant"]
      )
    ]
  ),
  ConjDesc(
    name: "Present Passive Indicative",
    tran: Translation(
      form: "$pronoun $aux [$eng_verb]ed",
      twoAux: true, aux: ["am", "are"]
    ),
    part: Infinitive,
    stem: @["a", "e", "i", "i", "i"],
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["*or", "*ris|*re", "*tur", "*mur", "*mini", "*ntur"]
      )
    ],
    exceptions: @[
      ( conj: @[First, Third_o], person: FirstSing, rule: "or" ),
      ( conj: @[Third_o, Third_io], person: SecondSing, rule: "eris|ere" ),
      ( conj: @[Third_o], person: ThirdPlur, rule: "untur" ),
      ( conj: @[Third_io, Fourth], person: ThirdPlur, rule: "iuntur" )
    ]
  ),
  ConjDesc(
    name: "Imperfect Passive Indicative",
    tran: Translation(
      form: "$pronoun $aux [$eng_verb]ed",
      twoAux: true, aux: ["was", "were"]
    ),
    part: Infinitive,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["*bar", "*baris|*bare", "*batur", "*bamur", "*bamini", "*bantur"]
      )
    ]
  ),
  ConjDesc(
    name: "Future Passive Indicative",
    tran: Translation(form: "$pronoun will be [$eng_verb]ed"),
    part: Infinitive,
    stem: @["a", "e", "e", "ie", "ie"],
    forms: @[
      (
        conj: @[First, Second],
        endings: ["*bor", "*beris|*bere", "*bitur", "*bimur", "*bimini", "*buntur"]
      ),
      (
        conj: @[Third_o, Third_io, Fourth],
        endings: ["iar", "*ris|*re", "*tur", "*mur", "*mini", "*ntur"]
      )
    ],
    exceptions: @[
      ( conj: @[Third_o], person: FirstSing, rule: "ar" )
    ]
  ),
  ConjDesc(
    name: "Perfect Passive Indicative",
    tran: Translation(form: "$pronoun have been [$eng_verb]ed",),
    part: PerfPassPart,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["us/a/um sum", "us/a/um es", "us/a/um est",
                  "i/ae/a sumus", "i/ae/a estis", "i/ae/a sunt"]
      )
    ]
  ),
  ConjDesc(
    name: "Pluperfect Passive Indicative",
    tran: Translation(form: "$pronoun had been [$eng_verb]ed"),
    part: PerfPassPart,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["us/a/um eram", "us/a/um eras", "us/a/um erat",
                  "i/ae/a eramus", "i/ae/a eratis", "i/ae/a erant"]
      )
    ]
  ),
  ConjDesc(
    name: "Future-Perfect Passive Indicative",
    tran: Translation(form: "$pronoun will have been [$eng_verb]ed"),
    part: PerfPassPart,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["us/a/um ero", "us/a/um eris", "us/a/um erit",
                  "i/ae/a erimus", "i/ae/a eritis", "i/ae/a erunt"]
      )
    ]
  ),
  ConjDesc(
    name: "Present Passive Periphrastic",
    tran: Translation(
      form: "$pronoun $option be [$eng_verb]ed",
      moreOptions: true, options: @["must", "have to"]
    ),
    part: Infinitive,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["*ndus/a/um sum", "*ndus/a/um es", "*ndus/a/um est",
                  "*ndi/ae/a sumus", "*ndi/ae/a estis", "*ndi/ae/a sunt"]
      )
    ]
  ),
  ConjDesc(
    name: "Future Passive Periphrastic",
    tran: Translation(form: "$pronoun will have to be [$eng_verb]ed"),
    part: Infinitive,
    forms: @[
      (
        conj: @[First, Second, Third_o, Third_io, Fourth],
        endings: ["*ndus/a/um ero", "*ndus/a/um eris", "*ndus/a/um erit",
                  "*ndi/ae/a erimus", "*ndi/ae/a eritis", "*ndi/ae/a erunt"]
      )
    ]
  )
]

### TODO: Participles are declined like NOUNS!!! they are not conjugated

converter toConjugation(str: string): Conjugation =
  case str.strip():
  of "1": First
  of "2": Second
  of "3o": Third_o
  of "3 o": Third_o
  of "3io": Third_io
  of "3 io": Third_io
  of "4": Fourth
  of "irr": Irregular
  else: Irregular

converter toVerb(str: string): Verb =
    
    #[ Interpret single line formed as such to make a single verb. E.g.
      "1  : Laudo, Laudare, Laudavi, Laudatus : Praise"
      "2  : Moneo, Monere, Monui, Monitus : Warn, Advise"
      "3o : Duco, Ducere, Duxi, Ductus : Lead"
      "3io: Capio, Capere, Cepi, Captus : ?"
      "4  : Audio, Audire, Audivi, Auditus : ?"]#

  let list = str.split('\t', 3) # divide into <conj> \t <4pp> \t <translations> \t unit

  result.conj = toConjugation list[0] # interpret string into `Conjugation`

  let fourPrinParts = list[1].split(", ")
  for i in PresActInd .. PerfPassPart:
    result.parts[i] = fourPrinParts[i].strip() # ensure no whitespace in part

  for translation in list[2].split({',', ';'}):
    result.trans.add translation.strip()
  
  if list[3].isEmptyOrWhitespace:
    result.unit = 0
  else:
    result.unit = uint8 parseInt(list[3])

proc findForm(v: Verb, tc: ConjDesc): ConjForm =
  # Select correct form
  for form in tc.forms:
    if v.conj in form.conj:
      result = form

  # Change forms forms to correct if there are exceptions
  for excepts in tc.exceptions:
    if v.conj in excepts.conj:
      result.endings[excepts.person] = excepts.rule

  # add stem endings
  let isStandardEnding = len(tc.stem) == 0
  let stemAdd =
    if isStandardEnding:
      StemAddition[v.conj]
    else:
      tc.stem[v.conj]
  for person in FirstSing .. ThirdPlur:
    let ending = result.endings[person]
    let replacement = ending.replace("*", stemAdd)
    result.endings[person] = replacement

proc getStem(v: Verb, pp: Part): string =
  # Get the stem from one of the four principal parts
  case pp:
  of Infinitive:
    let word = v.parts[Infinitive]
    word[0 ..< len(word)-3] # Remove last three letters in Inf
  of PerfActInd:
    let word = v.parts[PerfActInd].split('|').sample()
    word[0 ..< len(word)-1] # remove last letter i.e. 'i'
  of PerfPassPart:
    let word = v.parts[PerfPassPart].split('|').sample()
    word[0 ..< len(word) - 2] # remove "us"
  of PresActInd:
    v.parts[PresActInd] # no changes :)

const NoConjugation = ["-", "-", "-", "-", "-", "-"]
proc conjugate(v: Verb, tc: ConjDesc): Conjugated =
  let
    stem = getStem(v, tc.part)
    form = findForm(v, tc)
  
  if form.endings[0] == "-":
    return NoConjugation

  # Make all six conjugations for the tense
  for person in FirstSing .. ThirdPlur:
      var ifMultiple: seq[string]
      for ending in form.endings[person].split('|'): # this incase a verb has multiple 
        ifMultiple.add(stem & ending)
      result[person] = ifMultiple.join("|")

proc conjugate(v: Verb, tc: ConjDesc, p: Person): string =
  let
    stem = getStem(v, tc.part)
    form = findForm(v, tc).endings[p]
  
  if form == "-":
    return "-"

  # Make a single conjugation for the tense, based on 'p'
  var ifMultiple: seq[string]
  for ending in form.split('|'): # this incase a verb has multiple 
    ifMultiple.add(stem & ending)
  return ifMultiple.join("|")

proc fileToVerbs*(filename: string, unit: Slice[int] = 0..32): seq[Verb] =
  for line in lines(filename):
    try:
      if line[0] notin {'\t', '#'}:
        let verb = toVerb(line)
        if int(verb.unit) in unit:
          result.add verb
    except Exception:
      quit(line, 0)

# Randomness section
template rndConjDescIndex*(): int = rand(len(conjDescs) - 1)

template randomPerson*(): Person = rand(FirstSing..ThirdPlur)

proc randomConjugation*(v: Verb, cd: ConjDesc): SpecConj =
  result.person = randomPerson()
  result.verb = v.conjugate(cd, result.person)

proc choosePronoun*(person: Person): string =
  const eng_pronouns = [
    @["I"], @["you"], @["he", "she", "it"],
    @["we"], @["you (all)", "you all", "you (pl.)"], @["they"]
  ]
  return sample(eng_pronouns[person])

# s tsuff...

template presentAux*(cdi: int, person: Person): string =
  if conjDescs[cdi].tran.twoAux:
    case person:
    of FirstSing: conjDescs[cdi].tran.aux[0]
    else: conjDescs[cdi].tran.aux[1]
  else: ""
template presentOption*(cdi: int): string =
  if conjDescs[cdi].tran.moreOptions:
    sample(conjDescs[cdi].tran.options)
  else: ""


# For flashcards
import flashcard
proc flashcard*(v: Verb): Flashcard =
  var cdi = rndConjDescIndex()
  var sc = randomConjugation(v, conjDescs[cdi])
  while sc.verb == "-":
    cdi = rndConjDescIndex()
    sc = randomConjugation(v, conjDescs[cdi])
  
  new result
  result.q = sc.verb.capitalizeAscii()
  result.a = conjDescs[cdi].tran.form % [
    "pronoun", choosePronoun(sc.person),
    "eng_verb", sample(v.trans),
    "aux", presentAux(cdi, sc.person),
    "option", presentOption(cdi)
  ]
  result.i = fmt"""{conjDescs[cdi].name}, {sc.person}{'\n'}   {v.parts.join(", ")}{'\n'}"""
  