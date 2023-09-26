Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_144") 
options (java.parameters="-Xmx8g")

library(stringr)
library(dplyr)
library(openxlsx)
library(reshape2)

## set the working directory
setwd("C:\\Users\\Piyush\\Desktop\\Touro\\ClinicalRotations\\clinicaldistinction\\MSKCC Code\\")

## read in the training dataset

tmp_initial = read.xlsx("deidentified_hl_short.xlsx", detectDates = TRUE)
#tmp_initial$report_date = as.Date(tmp_initial$report_date, origin = "1899-12-30")

## todo: figure out conditional regular expressions, but in the meantime will do it computationally intensive via str_extract
tmp=cbind(tmp_initial, as.data.frame(str_split_fixed(tmp_initial$impression, ifelse(is.na(str_extract(tmp_initial$impression, "(?!1\\.\\d)(1\\..*)"))==FALSE, "(?!\\d\\.\\d|\\d\\. \\d\\.)(?=\\d\\.)(?<![A-Z-0-9]|\\d\\.)", "(?<!\\d\\.)(?<=\\.)"), n= Inf)))
tmp=melt(tmp, measure.vars=grep("^V", colnames(tmp)), na.rm=T)

## split out the lagging SUMMARY portion of impression & remelt dataframe
tmp=cbind(tmp, as.data.frame(str_split_fixed(tmp$value, "(?=SUMMARY:)", n=Inf)))
tmp$value = NULL
tmp$variable = NULL
tmp=melt(tmp, measure.vars=grep("^V", colnames(tmp)), na.rm=T)

## copy lexicon updates from here.

lex.aspect = "(lesion|adenopath|foc(i|us|al)|abnormal(ities|ity)?|disease|(hyper)?metabol|(FDG)?[-\\s]?(avid)|lymphoma|uptake|mass|tumor|intensity|finding|respon|Lugano|malignan|recurr|involvement|Deauville|\\w+ node(s)?|activity|nodule|thymic rebound|(interval|significant) change|reactive thymus|hodgkin|metast|FDG|neoplasm)"

tmp = tmp[with(tmp, grepl(paste0("(?i)", lex.aspect), value)), ]

## create four columns, one for each type of categorization
tmp$negativePET = ""
tmp$positivePET = ""
tmp$likelynegativePET = ""
tmp$likelypositivePET = ""

lex.aspect = "(lesion\\w*|adenopath\\w*|foc(i|us|al)|(?<!without |no )abnormal(ities|ity)?|disease|(?<!non|non |non-)(hyper)?metabol(ic|ism)|(FDG)?[-\\s]?(avid(ity)?)|lymphoma|uptake|mass|tumor|intensity|(?<!CT |CT-)finding\\w*|Lugano|malignan\\w*|recurr\\w*|involvement|Deauville|\\w+ nod(e|s|es|al|ular)*|activity|nodule(s)?|thym(ic|us) rebound|change|reactive thymus|hodgkin\\w*|metast\\w*|FDG|neoplasm|hepatic|splenic|chemotherapy|lymphadenopathy)"

lex.negative.pre = "(\\bno[t\\s]*((definite |PET/CT )?evidence (of|for)|\\s*sites|\\s*findings\\s*\\w*\\s*(for|of)?)?|resol(ve\\w*|ution)|complete response|non[-\\s*]?(specific)?|physiologic|unremarkable|(consistent with|probably|stable) treat(ment|ed)|diffuse\\w*|nonhypermetabolic)"
lex.negative.pre.long = "(resolution of)"
lex.negative.definitive = "(negative FDG[-\\s]*PET scan|complete metabolic response|interval resol\\w*|((are|more|is|\\W) likely|most suggestive)(\\s*\\w*\\s*){0,2}(infect\\w*|reactive|\\w*\\W*inflamma\\w*|physiologic|treat\\w*|\\w*\\s*\\w*\\s*G[\\s-]*CSF|trauma|fracture|post[\\s-]*therapy)|((?-i)No(?i)|without|with no)\\s*evidence(\\s*\\w*\\s*){1,2}(lymphoma|FDG|disease)|(?-i)Resolution of(?i)|(?-i)Resolved(?i))"
lex.negative.post = "((?<!not )(less than|below|\\bno(t)? greater|similar to|equivalent to)(\\s*\\w*\\s*){1,2}(liver|background|blood)|non[-\\s*]?specific|(consistent with|probably|confirmed)(\\s*\\w*\\s*){0,2}(reactive|\\w*itis|treat\\w*|inflamma\\w*|infect\\w*|G[-\\s]*CSF|rebound thym\\w*)|unremarkable|no longer evident|(infect\\w*\\s*|inflamma\\w*\\s*)consolidation|(without|with no)\\s*evidence(\\s*\\w*\\s*){1,2}(lymphoma)|PET[\\s-]*negative)"
lex.negative.pre.modifier = "((?-i)otherwise(?i)|(?-i)other than(?i)|interval (?!(?:resolution))\\w+|(?<!diffuse |diffusely )increas\\w* (?!(?:diffusely)|(?:non)|(?:diffuse))|(?-i)New (?!(?:non)|(?:left)|(?:right))\\w+(?i))"
lex.negative.post.modifier = "([,\\s]*\\bbut)"

negativePET <- function(PETreport) {
  return(ifelse ((grepl(paste0("(?i)",lex.negative.pre, "(?:\\W++(?!(?-1))\\w+){0,4}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) | 
                    grepl(paste0("(?i)",lex.negative.pre.long, "(?:\\W++(?!(?-1))\\w+){0,}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE)) & 
                   !grepl(paste0("(?i)", lex.negative.pre.modifier, "(?:\\W++(?!(?-1))\\w+){0,6}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) & 
                   !grepl(paste0("(?i)", lex.aspect, "(?:\\W++(?!(?-1))\\w+){0,2}\\s*", lex.negative.post.modifier), PETreport, ignore.case = TRUE, perl = TRUE) |
                   grepl(paste0("(?i)", lex.aspect, "(?:\\W++(?!(?-1))\\w+){0,}\\s*\\W*", lex.negative.post), PETreport, ignore.case = TRUE, perl = TRUE) |
                   grepl(paste0("(?i)", lex.negative.definitive), PETreport, ignore.case = TRUE, perl = TRUE), "negativePET", 0))
}

tmp$negativePET = negativePET(tmp$value)

lex.likelynegative.pre = "(diffuse|decreas\\w*\\s+(?!\\s*size)|minor|((minimal)\\s*(\\w*\\s*increase|residual))|(continu\\w*|further|overall|substantial\\w*|significant\\w*|marked\\w*|predominant\\w*) (decrease(d)?|diminish(ed|ing)?)\\s*\\w*|probably \\w* treat\\w*|(?-i)Decreas\\w*(?i)|low[-\\s]grade|interval(?!\\s*increase|\\s*stab\\w*|\\s*resol\\w*)|mild\\w*|minimal|(probably )?reactive|improv\\w*|brown fat|unchanged (?!(?:FDG))|slight\\w*|splenic)"
lex.likelynegative.short = "((?<!consistent with )(physiologic|rebound))"
lex.likelynegative.post = "(is decreased|brown (fat|adipose)|repopulation|likely secondary)"
lex.likelynegative.pre.definitive = "((probably|possibly) (reactive|\\w*\\W*inflamma\\w*|physiologic|treat\\w*)|non[-\\s]*specific)"
lex.likelynegative.post.definitive = "((probably|possibly)(\\s*\\w*\\s*){0,2}(reactive|\\w*\\W*inflamma\\w*|physiologic|treat\\w*|\\w*\\s*\\w*\\s*G[\\s-]*CSF|thym\\s*|post[\\s-]*trauma\\w*)|non[-\\s]*specific|indeterminate)"
lex.likelynegative.definitive = "((probably|may be) (infect\\w*|inflamma\\w*|reactive|\\w*\\s*thymic rebound|(activated )?brown fat)|less likely lymphomatous|continued improvement|(possibly)(\\s*\\w*\\s*){0,2}(treat\\w*|inflamma\\w*|reactive|infect\\w*|(?!ab)\\w*\\s*normal|\\w*itis|G[\\s-]*CSF|splenic|trauma|fracture))"
lex.likelynegative.pre.modifier = "(((?<!mild |mildly |diffuse |diffusely |minimal interval ))(persistent|increas\\w*)(?!\\s*\\w*\\s*remains|\\s*mild\\w*|minimal\\w*)|stable|(Complete|and) resolution (of)?)"
lex.likelynegative.post.modifier = "((?<!(?:no)|(?:not)) increase\\w* \\w*\\s*intensity|possibly disease|(not|non|minimal)|(?<!not )(is|now)?\\s*(less than|below|no greater)(\\s*\\w*\\s*){1,2}(liver|background|blood)|consistent with treat\\w*)"
lex.likelynegative.definitive.modifier = "(suspicious for|without( abnormal)?[-\\s]*(FDG)|non[\\s-]*(hypermetabolic|FDG))"

likelynegativePET <- function(PETreport) {
  return(ifelse (((grepl(paste0("(?i)",lex.aspect, "(?:\\W++(?!(?-9))\\w+){0,10}\\s*\\W*", lex.likelynegative.post), PETreport, ignore.case = TRUE, perl = TRUE) |
                     grepl(paste0("(?i)",lex.likelynegative.pre, "(?:\\W++(?!(?-1))\\w+){0,7}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) | 
                     grepl(paste0("(?i)",lex.likelynegative.short, "(?:\\W++(?!(?-1))\\w+){0,2}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) |
                     grepl(paste0("(?i)",lex.aspect, "(?:\\W++(?!(?-1))\\w+){0,2}\\s*", lex.likelynegative.short), PETreport, ignore.case = TRUE, perl = TRUE) |
                     (grepl(paste0("(?i)",lex.negative.pre, "(?:\\W++(?!(?-1))\\w+){0,3}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) & 
                        grepl(paste0("(?i)", lex.aspect, "(?:\\W++(?!(?-1))\\w+){0,2}\\s*", lex.negative.post.modifier), PETreport, ignore.case = TRUE, perl = TRUE))) & 
                    !grepl(paste0("(?i)", lex.aspect, "(?:\\W++(?!(?-1))\\w+){0,5}\\s*\\W*", lex.likelynegative.post.modifier), PETreport, ignore.case = TRUE, perl = TRUE) &
                    !grepl(paste0("(?i)", lex.likelynegative.pre.modifier, "(?:\\W++(?!(?-1))\\w+){0,4}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) | 
                    grepl(paste0("(?i)", lex.aspect, "(?:\\W++(?!(?-9))\\w+){0,7}\\s*\\W*", lex.likelynegative.post.definitive), PETreport, ignore.case = TRUE, perl = TRUE) | 
                    grepl(paste0("(?i)", lex.likelynegative.definitive), PETreport, ignore.case = TRUE, perl = TRUE)) & 
                   !grepl(paste0("(?i)", lex.likelynegative.definitive.modifier), PETreport, ignore.case = TRUE, perl = TRUE), "likelynegativePET", 0))
}

tmp$likelynegativePET = likelynegativePET(tmp$value)

lex.positive.aspect = "((?<!(?:non))\\s(hyper)?metabolic|(?<!(?:non)|(?:slightly))\\sFDG[-\\s](avid\\w*|uptake)|involvement|viable (tumor|disease)|(?<!(?:possibly ))lymphoma|uptake|(Deauville|Lugano) (3|4|5)|focal activity)"
lex.positive.pre = "((?<!(?:no ))new(ly)?|increas(ing|ed|e in)|consistent with|continue to|no sites|(?-i)Hypermetabolic(?i)|more|persistent|progression|high[-\\s]*grade)"
lex.positive.pre.definitive = "((?<!(?:no )|(?:no evidence of )|(?:or )|(?:no definite ))new (FDG|hypermetabolic)|(suspicious for|consistent with) (malignancy|metas\\w*|viable|lymphomatous|involvement))"
lex.positive.post.definitive = "((?<!(?:no )|(?:no evidence of ))new (FDG|hypermetabolic)|(?<!(?:no )|(?:no evidence of )|(?:not ))(suspicious for|represents|consistent with) (malignancy|\\w*\\s*lymphoma|viable tumor|involvement)|(?<!(?:not ))greater (than|in)(\\s*\\w*\\s*){1,2}liver|(?<!no |minimal )(clear )?residual abnormal\\w*\\s*\\w*\\s*remain\\w*)"
lex.positive.definitive.modifier = "((represent|probably|likely|may be|suspicious for|consistent|possibly|confirmed)(\\s*\\w*\\s*){0,3}(treat\\w*|inflamma\\w*|reactive|infect\\w*|(?!ab)\\w*\\s*normal|\\w*itis|G[\\s-]*CSF|splenic|trauma|fracture|rebound thym\\w*|post[\\s-]*therapy|brown[\\s-]*fat)|less likely (malignancy|\\s*\\w*\\s*lymphoma)|(no findings)|negative FDG[-\\s]*PET scan|(less than|below|no greater)(\\s*\\w*\\s*){1,2}(liver|background)|effusion|(mild\\w*|\\bno) (hypermetabolic|FDG|uptake)|interval normalization|too small|resol\\w*|no longer evident|uncertain significance|post[\\s-]*radiation changes|non[\\s-]*specific|indeterminate)"
lex.positive.post = "(increased|newly[-\\s]*visualized for malignancy)"
lex.positive.pre.modifier = "(\\bno[nt\\s]*|mild\\w*|no (([\\w\\W\\s]*evidence (of|for))|(\\w*\\s*sites)|FDG)|resol\\w*( of)?|decreas\\w*|treat(ment|ed)(?!\\s*\\w*\\s*remains)|non[\\s-]*specific|physiologic|adipose|diffuse\\w*|minimal|slight\\w*|resolved|complete|normal\\w*|splenic|low[\\s-]grade)"
lex.positive.post.modifier = "(treat(ment|ed)|brown (fat|adipose)|non[\\s-]*specific|(probably|possibly) (\\w*\\s*physiologic|\\w*\\s*\\w*itis|reactive|\\w*\\s*radiation|benign|\\w*\\s*urine)|(less than|below|similar to) (liver|\\w*\\s*background|\\w*\\s*hepatic|\\w*\\s*blood)|(likely|probably|may be|possibl\\w*|considered)\\s*\\w*\\s*(inflamma\\w*|infect\\w*|G[-\\s]*CSF|secondary)|\\w* resol\\w*|splenic|no longer (evident)|without\\s*\\w*\\s*(CT|uptake)|less likely\\s*\\w*\\s*lymphoma|(?<!less likely |versus )(infect\\w*)|(?<!less likely |versus )(inflamma\\w*)|PET[\\s-]*negative|thyroid)"
lex.positive.modifier.exception = "(((?<!(?:not ))(greater (than|in)|above) (?:(\\b\\w*\\s*)){0,3}liver)|(?-i)^(\\d\\W\\s*)?(Hypermetabolic|FDG[\\s-]*avid)(?i)|consistent with lymphoma)"

positivePET <- function(PETreport) {
  return(ifelse ((((grepl(paste0("(?i)", lex.positive.pre, "(?:\\W++(?!(?-1))\\w+){0,4}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) |
                      grepl(paste0("(?i)", lex.positive.aspect), PETreport, ignore.case = TRUE, perl = TRUE) |
                      grepl(paste0("(?i)", lex.aspect, "(?:\\W++(?!(?-7))\\w+){0,2}\\s*\\W*", lex.positive.post), PETreport, ignore.case = TRUE, perl = TRUE))  & 
                     !grepl(paste0("(?i)", lex.positive.pre.modifier, "(?:\\W++(?!(?-3))\\w+){0,6}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) & 
                     !grepl(paste0("(?i)", lex.aspect, "(?:\\W++(?!(?-9))\\w+){0,8}\\s*\\W*", lex.positive.post.modifier), PETreport, ignore.case = TRUE, perl = TRUE) |
                     (grepl(paste0("(?i)", lex.positive.pre.definitive, "(?:\\W++(?!(?-1))\\w+){0,2}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) |
                        grepl(paste0("(?i)", lex.aspect, "(?:\\W++(?!(?-1))\\w+){0,2}\\s*\\W*", lex.positive.post.definitive), PETreport, ignore.case = TRUE, perl = TRUE))) &
                    !grepl(paste0("(?i)", lex.likelynegative.definitive), PETreport, ignore.case = TRUE, perl = TRUE) & 
                    !grepl(paste0("(?i)", lex.negative.definitive), PETreport, ignore.case = TRUE, perl = TRUE)) & 
                   !grepl(paste0("(?i)", lex.positive.definitive.modifier), PETreport, ignore.case = TRUE, perl = TRUE) |
                   grepl(paste0("(?i)", lex.positive.modifier.exception), PETreport, ignore.case = TRUE, perl = TRUE), "positivePET", 0))
}

tmp$positivePET = positivePET(tmp$value)

lex.likelypositive.pre = "(stable|increas|mixed|questionable|prominent|slight\\w*|(?<!(?:(marked))|(?:(significant))|(?:(prominent))|(?:(substantially)))\\sdecreas\\w*|interval increase|(?<!\\bno\\s|\\bno new\\s)suspicious|residual)"
lex.likelypositive.post = "(inflammat\\w*|infect\\w*|stable)"
lex.likelypositive.pre.modifier = "((not|non|no (new|residual))[-\\s]*(FDG)?[-\\s]*avid|resol\\w*( of)?|treat\\w*|no (abnormal|longer|residual|new sites)|(not|non|minimal|no)[-\\s]*(FDG)|non[-\\s]*specific|diffuse|no (evidence|\\w*\\s*findings|definite) \\w*|no new|not\\s*\\w*\\s*suspicious)"
lex.likelypostive.post.modifier = "((not|non)[-\\s]*(FDG)?[-\\s]*avid|non[-\\s]*specific|rebound|hyperplasia|(not|non|minimal|without( abnormal)?)[-\\s]*(FDG)|non[-\\s]*specific|(less than|below|equal|not (greater|above)|similar to|equal to|equivalent to)(\\s*\\w*\\s*){0,2}(liver|background|hepatic|blood)|(likely|probably|may be|possibl\\w*|consistent with|considered|represent\\w*)\\s*\\w*\\s*(physiologic|brown (fat|adipose)|reactive|inflamma\\w*|infect\\w*|G[-\\s]*CSF|post[\\s-]*trauma\\w*|(post)?[\\s-]*treat\\w*)|without evidence(\\s*\\w*\\s*){0,2}lymphoma|(infect\\w*\\s*|inflamma\\w*\\s*)consolidation|less than 1\\s*cm|not\\s*\\w*\\s*suspicious)"
lex.likelypositive.definitive = "(interval increase|less likely (inflamma\\w*|infect\\w*)|uncertain significance|without\\s*\\w*\\s*CT|(?<!not )(\\blikely|possibly|may)(\\s*\\w*\\s*){0,2}(disease versus|lymphoma|malig\\w*)|(malig\\w*|disease)(\\s*\\w*\\s*)can[\\s]*not(\\s*\\w*\\s*)be(\\s*\\w*\\s*)excluded)"

likelypositivePET <- function(PETreport) {
  return(ifelse (grepl(paste0("(?i)",lex.positive.pre, "(?:\\W++(?!(?-1))\\w+){0,3}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) & 
                   grepl(paste0("(?i)",lex.aspect, "(?:\\W++(?!(?-8))\\w+){0,4}\\s*", lex.likelypositive.post), PETreport, ignore.case = TRUE, perl = TRUE) & 
                   !grepl(paste0("(?i)",lex.aspect, "(?:\\W++(?!(?-1))\\w+){0,3}\\s*\\W*", lex.likelypostive.post.modifier), PETreport, ignore.case = TRUE, perl = TRUE) |
                   grepl(paste0("(?i)",lex.likelypositive.pre, "(?:\\W++(?!(?-1))\\w+){0,4}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) & 
                   !grepl(paste0("(?i)",lex.likelypositive.pre.modifier, "(?:\\W++(?!(?-1))\\w+){0,4}\\s*", lex.aspect), PETreport, ignore.case = TRUE, perl = TRUE) & 
                   !grepl(paste0("(?i)",lex.aspect, "(?:\\W++(?!(?-1))\\w+){0,8}\\s*\\W*", lex.likelypostive.post.modifier), PETreport, ignore.case = TRUE, perl = TRUE) |
                   grepl(paste0("(?i)",lex.likelypositive.definitive), PETreport, ignore.case = TRUE, perl = TRUE) , "likelypositivePET", 0))
}

tmp$likelypositivePET = likelypositivePET(tmp$value)

## paste lexicon updates till here


tmp$value = NULL
tmp$variable = NULL

tmp=melt(tmp, measure.vars=grep("PET$", colnames(tmp)), na.rm=T)
tmp[tmp$value == "0", ]$value = ""

tmp = dcast(tmp, ... ~ variable, fun.aggregate = function (x) paste0(x,collapse=" "), value.var = "value")

tmp$negativePET = str_trim(tmp$negativePET)
tmp$likelynegativePET = str_trim(tmp$likelynegativePET)
tmp$positivePET = str_trim(tmp$positivePET)
tmp$likelypositivePET = str_trim(tmp$likelypositivePET)

tmp_positive = tmp[tmp$positivePET != "", ]
tmp = tmp[tmp$positivePET == "", ]
tmp_positive$impressionStatus = "positive"

tmp_likelypositive = tmp[tmp$likelypositivePET != "", ]
tmp = tmp[tmp$likelypositivePET == "", ]
tmp_likelypositive$impressionStatus = "likelypositive"

tmp_likelynegative = tmp[tmp$likelynegativePET != "", ]
tmp = tmp[tmp$likelynegativePET == "", ]
tmp_likelynegative$impressionStatus = "likelynegative"

tmp_negative = tmp[tmp$negativePET != "", ]
tmp = tmp[tmp$negativePET == "", ]
tmp_negative$impressionStatus = "negative"

tmp$impressionStatus = "manualcheck"

tmp = rbind(tmp, tmp_positive, tmp_negative, tmp_likelynegative, tmp_likelypositive)

tmp = tmp[, !(names(tmp) %in% c("negativePET", "positivePET", "likelynegativePET", "likelypositivePET"))]

if(nrow(tmp_initial) > nrow(tmp))
{
  tmp_initial$impressionStatus = ""
  tmp = rbind(tmp, anti_join(tmp_initial, tmp, by=c("mrn", "report_date", "report_text")))
  tmp[tmp$impressionStatus == "", ]$impressionStatus = "manualcheck"
  write.xlsx(tmp, "traningdataset_pet_scans_eval.xlsx")
} else
{
  write.xlsx(tmp, "traningdataset_pet_scans_eval.xlsx")
}


