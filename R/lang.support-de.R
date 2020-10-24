# Copyright 2010-2020 Meik Michalke <meik.michalke@hhu.de>
#
# This file is part of the R package koRpus.lang.de.
#
# koRpus.lang.de is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# koRpus.lang.de is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with koRpus.lang.de.  If not, see <http://www.gnu.org/licenses/>.


# this is an internal file providing language support.
# please refer to inst/README.languages for details

#' Language support for German
#' 
#' This function adds support for German to the koRpus package. You should not
#' need to call it manually, as that is done automatically when this package is
#' being loaded.
#' 
#' In particular, this function adds the following:
#' \itemize{
#'  \item \code{lang}: The additional language "de" to be used with koRpus
#'  \item \code{treetag}: The additional preset "de", implemented according to the respective TreeTagger[1] script
#'  \item \code{POS tags}: An additional set of tags, implemented using the documentation for the corresponding
#'    TreeTagger parameter set[2]
#' }
#' Hyphenation patterns are provided by means of the \code{\link[sylly.de:hyph.support.de]{sylly.de}} package.
#'
#' @param ... Optional arguments for \code{\link[koRpus:set.lang.support]{set.lang.support}}.
#' @references
#' [1] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/}
#'
#' [2] \url{http://www.cis.uni-muenchen.de/~schmid/tools/TreeTagger/data/stts_guide.pdf}
#' @export
#' @importFrom koRpus set.lang.support
#' @examples
#' lang.support.de()

lang.support.de <- function(...) {
  koRpus::set.lang.support("treetag",
    list("de"=list(
      ## preset: "de"
      lang="de",
      encoding="UTF-8",
      preset=function(TT.cmd, TT.bin, TT.lib, unix.OS){
        TT.tokenizer  <- file.path(TT.cmd, "utf8-tokenize.perl")
        TT.abbrev     <- file.path(TT.lib, "german-abbreviations")
        TT.params     <- file.path(TT.lib, "german.par")
        TT.lexicon    <- file.path(TT.lib, "german-lexicon.txt")
        TT.lookup     <- file.path(TT.cmd, "lookup.perl")
        TT.filter     <- file.path(TT.cmd, "filter-german-tags")
        if(isTRUE(unix.OS)){
          # preset for unix systems
          return(
            list(
              TT.tokenizer      = TT.tokenizer,
              TT.tagger         = file.path(TT.bin, "tree-tagger"),
              TT.abbrev         = TT.abbrev,
              TT.params         = TT.params,
              TT.lexicon        = TT.lexicon,
              TT.lookup         = TT.lookup,
              TT.filter         = TT.filter,

              TT.tknz.opts      = c(),
              TT.lookup.command = paste("perl", TT.lookup, TT.lexicon, "|"),
              TT.filter.command = paste("|", TT.filter)
            )
          )
        } else {
          # preset for windows systems
          return(
            list(
              TT.tokenizer      = TT.tokenizer,
              TT.tagger         = file.path(TT.bin, "tree-tagger.exe"),
              TT.abbrev         = TT.abbrev,
              TT.params         = TT.params,
              TT.lexicon        = c(),
              TT.lookup         = c(),
              TT.filter         = c(),

              TT.tknz.opts      = c(),
              TT.lookup.command = c(),
              TT.filter.command = c()
            )
          )
        }
      })
    ),
    ...
  )

  koRpus::set.lang.support("kRp.POS.tags",
    ## tag and class definitions
    # de -- german
    list("de"=list(
      tag.class.def.words=matrix(c(
        "ADJ", "adjective", "Adjektiv",
        "ADJA", "adjective", "attributives Adjektiv",
        "ADJD", "adjective", "adverbiales oder pr\u00e4dikatives Adjektiv",
        "ADV", "adverb", "Adverb",
        "AP", "adposition", "Adposition",
        "APPR", "preposition", "Pr\u00e4position; Zirkumposition links",
        "APPRART", "preposition", "Pr\u00e4position mit Artikel",
        "APPO", "postposition", "Postposition",
        "APZR", "circumposition", "Zirkumposition rechts",
        "ART", "article", "Artikel",
        "CARD", "number", "Kardinalzahl",
        "FM", "foreign", "fremdsprachliches Material",
        "ITJ", "interjection", "Interjektion",
        "KO", "conjunction", "Konjunktion",
        "KOUI", "conjunction", "unterordnende Konjunktion mit zu und Infinitiv",
        "KOUS", "conjunction", "unterordnende Konjunktion mit Satz",
        "KON", "conjunction", "nebenordnende Konjunktion",
        "KOKOM", "conjunction", "Vergleichspartikel ohne Satz",
        "NE", "name", "Eigenname",
        "NN", "noun", "Nomen",
        "P", "pronoun", "Pronomen",
        "PDS", "pronoun", "substituierendes Demonstrativpronomen",
        "PDAT", "pronoun", "attribuierendes Demonstrativpronomen",
        "PIS", "pronoun", "substituierendes Indefinitpronomen",
        "PIAT", "pronoun", "attribuierendes Indefinitpronomen ohne Determiner",
        "PIDAT", "pronoun", "attribuierendes Indefinitpronomen mit Determiner",
        "PPER", "pronoun", "irreflexibles Personalpronomen",
        "PPOSS", "pronoun", "substituierendes Possesivpronomen",
        "PPOSAT", "pronoun", "attribuierendes Possesivpronomen",
        "PRELS", "pronoun", "substituierendes Relativpronomen",
        "PRELAT", "pronoun", "attribuierendes Relativpronomen",
        "PRF", "pronoun", "reflexibles Personalpronomen",
        "PWS", "pronoun", "substituierendes Interrogativpronomen",
        "PWAT", "pronoun", "attribuierendes Interrogativpronomen",
        "PWAV", "pronoun", "adverbiales Interrogativ- oder Relativpronomen",
        "PAV", "pronoun", "Pronominaladverb",
        "PROAV", "pronoun", "Pronominaladverb", # this is actually a bug in earlier TreeTagger versions!
        "PTK", "particle", "Partikel",
        "PTKZU", "particle", "zu vor Infinitiv",
        "PTKNEG", "particle", "Negationspartikel",
        "PTKVZ", "particle", "abgetrennter Verbzusatz",
        "PTKANT", "particle", "Antwortpartikel",
        "PTKA", "particle", "Partikel bei Adjektiv oder Adverb",
        "TRUNC", "composition", "Kompositions-Erstglied",
        "V", "verb", "Verb",
        "VVFIN", "verb", "finites Verb, voll",
        "VVIMP", "verb", "Verb, Imperativ, voll",
        "VVINF", "verb", "Verb, Infinitiv, voll",
        "VVIZU", "verb", "Verb, Infinitiv mit zu, voll",
        "VVPP", "verb", "Verb, Partizip Perfekt, voll",
        "VAFIN", "verb", "finites Verb, aux",
        "VAIMP", "verb", "Verb, Imperativ, aux",
        "VAINF", "verb", "Verb, Infinitiv, aux",
        "VAPP", "verb", "Verb, Partizip Perfekt, aux",
        "VMFIN", "verb", "finites Verb, modal",
        "VMINF", "verb", "Verb, Infinitiv, modal",
        "VMPP", "verb", "Verb, Partizip Perfekt, modal",
        "XY", "nonword", "Nichtwort"
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
      tag.class.def.punct=matrix(c(
        "$,", "comma", "Komma",
        "$(", "punctuation", "satzinterne Interpunktion"
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc"))),
      tag.class.def.sentc=matrix(c(
        "$.", "fullstop", "satzbeendende Interpunktion"
        ), ncol=3, byrow=TRUE, dimnames=list(c(),c("tag","wclass","desc")))
      )
    ),
    ...
  )
}

# this internal, non-exported function causes the language support to be
# properly added when the package gets loaded
#' @importFrom sylly.de hyph.support.de
.onAttach <- function(...) {
  lang.support.de()
  sylly.de::hyph.support.de()
}
