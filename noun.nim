import std/[strutils]

type
  Declension = enum
    First, Second, Third, Third_i, Fourth, Fifth  
  Gender = enum
    Masculine, Feminine, Neuter, MascFem
  Noun* = object
    declension: Declension
    gender: Gender
    nom, gen: string
    trans: seq[string]
    unit: uint8
    exception: seq[Except]
  ExceptKind = enum
    PluralOnly, SingularOnly, Irregular
  Except = object
    case kind: ExceptKind
    of Irregular:
      irr: tuple[c: Case, f: string]
    else: discard

  DeclDesc = object
    declension: Declension
    forms: seq[DeclForm]
  DeclForm = tuple
    genders: seq[Gender]
    endings: array[12, string]
    identical: seq[seq[Case]]
  Case = enum
    NomSg = "Nominative Singular", GenSg = "Genitive Singular", DatSg = "Dative Singular", AccSg = "Accusitive Singular", AblSg = "Ablative Singular", VocSg = "Vocative Singular",
    NomPl = "Nominative Plural", GenPl = "Genitive Plural", DatPl = "Dative Plural", AccPl = "Accusitive Plural", AblPl = "Ablative Plural", VocPl = "Vocative Plural"
  Translation = tuple[kind: Case, usage: string]

const
  Singular = {NomSg .. VocSg}
  Plural = {NomPl .. VocPl}

proc `[]`[T](a: openArray[T], i: Case or Declension): T = a[i.int]
proc `[]=`[T](a: var openArray[T], i: Case or Declension, v: T) = a[i.int] = v

const tranDative = ["for", "to"]
const tranAblative = ["from", "with", "by", "in"]
const translations = [
  (kind: NomSg, usage: "[$eng_noun] (subject)"),
  (kind: GenSg, usage: "of [$eng_noun]"),
  (kind: DatSg, usage: "$dat [$eng_noun] (indirect object)"),
  (kind: AccSg, usage: "[$eng_noun] (direct object)"),
  (kind: AblSg, usage: "$abl [$eng_noun]"),
  (kind: VocSg, usage: "O [$eng_noun]"),

  (kind: NomPl, usage: "[$eng_noun]s (subject)"),
  (kind: GenPl, usage: "of [$eng_noun]s"),
  (kind: DatPl, usage: "$dat [$eng_noun]s (indirect object)"),
  (kind: AccPl, usage: "[$eng_noun]s (direct object)"),
  (kind: AblPl, usage: "$abl [$eng_noun]s"),
  (kind: VocPl, usage: "O [$eng_noun]s")
]
const declDescs = [
  DeclDesc(
    declension: First,
    forms: @[
      (
        genders: @[Feminine, Masculine],
        endings: ["a", "ae", "ae", "am", "a", "a",
                  "ae", "arum", "is", "as", "is", "ae"],
        identical: @[
          @[NomSg, AblSg, VocPl], @[GenSg, DatSg, VocPl], @[DatPl, AblPl]
        ]
      )
    ],
  ),
  DeclDesc(
    declension: Second,
    forms: @[
      (
        genders: @[Masculine],
        endings: ["ius", "i",    "o",  "um", "o",  "", # for verbs ending in "ius"
                  "i", "orum", "is", "os", "is", "i"],
        identical: @[
          @[GenSg, NomPl, VocPl], @[DatSg, AblSg], @[DatPl, AblPl]
        ]
      ),
      (
        genders: @[Masculine],
        endings: ["us", "i",    "o",  "um", "o",  "e",
                  "i", "orum", "is", "os", "is", "i"],
        identical: @[
          @[GenSg, NomPl, VocPl], @[DatSg, AblSg], @[DatPl, AblPl]
        ]
      ),
      (
        genders: @[Neuter],
        endings: ["um", "i",    "o",  "um", "o",  "um", # e in vocative, unless ending in "ius" = "i"
                  "a",  "orum", "is", "a",  "is", "a"],
        identical: @[
          @[NomSg, AccSg, VocSg], @[NomPl, AccPl, VocPl], @[DatSg, AblSg], @[DatPl, AblPl]
        ]
      )
    ]
  ),
  DeclDesc(
    declension: Third,
    forms: @[
      (
        genders: @[Masculine, Feminine],
        endings: ["*", "is", "i", "em", "e", "*",
                  "es", "um", "ibus", "es", "ibus", "es"],
        identical: @[
          @[NomSg, VocSg], @[NomPl, AccPl, VocPl]
        ]
      ),
      (
        genders: @[Neuter],
        endings: ["*", "is", "i", "*", "e", "*",
                  "a", "um", "ibus", "a", "ibus", "a"],
        identical: @[
          @[NomSg, AccSg, VocSg], @[NomPl, AccPl, VocPl], @[DatPl, AblPl]
        ]
      )
    ]
  ),
  DeclDesc(
    declension: Third_i,
    forms: @[
      (
        genders: @[Masculine, Feminine],
        endings: ["*", "is", "i", "em", "e", "*",
                  "es", "ium", "ibus", "es", "ibus", "es"],
        identical: @[
          @[NomSg, VocSg], @[NomPl, AccPl, VocPl], @[DatPl, AblPl]
        ]
      ),
      (
        genders: @[Neuter],
        endings: ["*", "is", "i", "*", "e", "*",
                  "ia", "ium", "ibus", "ia", "ibus", "ia"],
        identical: @[
          @[NomSg, AccSg, VocSg], @[NomPl, AccPl, VocPl], @[DatPl, AblPl]
        ]
      )
    ]
  ),
  DeclDesc(
    declension: Fourth,
    forms: @[
      (
        genders: @[Masculine, Feminine],
        endings: ["us", "us", "ui", "um", "u", "us",
                  "us", "uum", "ibus", "us", "ibus", "us"],
        identical: @[
          @[NomSg, GenSg, VocSg, NomPl, AccPl, VocPl], @[DatPl, AblPl]
        ]
      ),
      (
        genders: @[Neuter],
        endings: ["u", "us", "u", "u", "u", "u",
                  "ua", "uum", "ibus", "ua", "ibus", "ua"],
        identical: @[
          @[NomSg, DatSg, AccSg, AblSg, VocSg], @[NomPl, AccPl, VocPl], @[DatPl, AblPl]
        ]
      )
    ]
  ),
  DeclDesc(
    declension: Fifth,
    forms: @[
      (
        genders: @[MascFem, Feminine],
        endings: ["es", "ei", "ei", "em", "e", "es",
                  "es", "erum", "ebus", "es", "ebus", "es"],
        identical: @[
          @[NomSg, VocSg, NomPl, AccPl, VocPl], @[GenSg, DatSg], @[DatPl, AblPl]
        ]
      )
    ]
  ) 
]

converter toDeclension(str: string): Declension =
  case str:
  of "1": First
  of "2": Second
  of "3": Third
  of "3i": Third_i
  of "4": Fourth
  of "5": Fifth
  else:
    raise newException(Exception, "Declension Failure")

converter toGender(str: string): Gender =
  case str:
  of "m": Masculine
  of "f": Feminine
  of "n": Neuter
  of "mf": MascFem
  else:
    raise newException(Exception, "Gender Failure")

converter toCase(str: string): Case =
  case str.toLowerAscii():
  of "nomsg": NomSg
  of "gensg": GenSg
  of "datsg": DatSg
  of "accsg": AccSg
  of "ablsg": AblSg
  of "vocsg": VocSg
  of "nompl": NomPl
  of "genpl": GenPl
  of "datpl": DatPl
  of "accpl": AccPl
  of "ablpl": AblPl
  of "vocpl": VocPl
  else:
    raise newException(Exception, "Case Failure")

proc toNoun(str: string): Noun =
  let list = str.split('\t')
  
  result.declension = toDeclension list[0]

  let nom_gen = list[1].split(", ")
  result.nom = nom_gen[0]
  result.gen = nom_gen[1]

  result.gender = toGender list[2]

  result.trans = list[3].split({',', ';'})
  for item in result.trans.mitems:
    item = item.strip()
  
  result.unit = uint8 parseInt list[4]

  if len(list) == 6:

    for item in list[5].split({',', ';'}):
      let item = item.strip()
      if item.startsWith('!'):
        case item.toLowerAscii():
        of "!singular-only": result.exception.add Except(kind: SingularOnly)
        of "!plural-only": result.exception.add Except(kind: PluralOnly)
        else:
          raise newException(Exception, "ExceptKind Failure")
      else:
        let list = item.split(':', 1)
        result.exception.add Except(
          kind: Irregular,
          irr: (c: toCase list[0].strip(), f: list[1].strip())
        )

proc fileToNouns*(filename: string, unit: Slice[int] = 0..32): seq[Noun] =
  for line in lines(filename):
    try:
      result.add toNoun line
    except:
      echo line

proc findForm(n: Noun): DeclForm =
  for form in declDescs[n.declension].forms:
    if n.gender in form.genders:
      return form

proc decline(n: Noun, c: Case): string =

  # check all exception first
  for exp in n.exception:
    case exp.kind:
    of PluralOnly:
      if c in Singular:
        return "-"
    of SingularOnly:
      if c in Plural:
        return "-"
    of Irregular:
      if c == exp.irr.c:
        return exp.irr.f
  
  case c:
  of NomSg: return n.nom
  of GenSg: return n.gen
  else:
    echo "decline error?"
    let form = findForm(n)
  
    if form.endings[c] == "*": 
      return n.nom # Start indicates "same as nominative"
  
    # noun is formed from taking away characters from genitive case
    var stem = n.gen.capitalizeAscii()
    stem.removeSuffix(form.endings[GenSg])
    
    return stem & form.endings[c]


import std/[random], flashcard
randomize()
template randomCase(): Case =
  rand(Case.low .. Case.high)

proc formTranslation(n: Noun, c: Case): string =
  translations[c].usage % [
    "eng_noun", sample(n.trans),
    "abl", sample(tranAblative),
    "dat", sample(tranDative)
  ]

proc flashcard*(n: Noun): Flashcard =
  let c: Case = randomCase()
  new result
  result.q = decline(n, c)

  var possibleAnswers: seq[string]
  for identList in findForm(n).identical:
    if c in identList:
      for identCase in identList:
        possibleAnswers.add $identCase & ": " & formTranslation(n,identCase)
          
  if len(possibleAnswers) == 0:
    result.a = $c & ": " & formTranslation(n,c)
  else:
    result.a = possibleAnswers.join("\n")
  result.i = $n.gender & " Noun"
