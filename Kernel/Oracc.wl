BeginPackage["ChristopherWolfram`CuneiformTools`Oracc`"];

Begin["`Private`"];

Needs["ChristopherWolfram`CuneiformTools`"]


$OraccBase = "oracc.org";


(* General *)

oraccProjectName[proj_?StringQ] := proj
oraccProjectName[{proj_,subproj_}] := proj<>"-"<>subproj

oraccProjectPath[proj_?StringQ] := proj
oraccProjectPath[{proj_,subproj_}] := proj<>"/"<>subproj


(* Metadata *)

oraccProjectMetadata[projSpec_]:=
	URLRead[<|
		"Scheme" -> "https",
		"Domain" -> $OraccBase,
		"Path" -> {"json", oraccProjectName[projSpec]<>".zip"}
	|>]

parseOraccProject[str_] := Replace[StringSplit[str,"/"],{s_}:>s]


(* Catalog *)

oraccCatalogPath[proj_?StringQ] := FileNameJoin[{proj,"catalogue.json"}]
oraccCatalogPath[{proj_,subproj_}] := FileNameJoin[{proj,subproj,"catalogue.json"}]

oraccProjectCatalog[projSpec_]:=
	Enclose@Import[
		ConfirmBy[oraccProjectMetadata[projSpec],#["StatusCode"]===200&],
		{"ZIP",oraccCatalogPath[projSpec],"RawJSON"}
	]["members"]


(* HTML parsing *)

oraccObjectHTML[projSpec_, obj_] :=
	URLRead[HTTPRequest[<|
		"Domain"->$OraccBase,
		"Path"->{oraccProjectPath[projSpec],obj,"html"}
	|>]]

oraccObjectParseHTML[html_] :=
	Module[{xml, texts},
		xml = Import[html, {"HTML","XMLObject"}];
		texts = Cases[xml, XMLElement["div",KeyValuePattern["class"->"text"],t_]:>t, Infinity];
		Catenate[parseHTMLText/@texts]
	]

classContainsPattern[str_] := _?(MemberQ[StringSplit[#," "],str]&)

getColumnsLineNumber[cols_] :=
	Module[{lnumColumns},
		lnumColumns = Cases[cols, XMLElement[_, KeyValuePattern["class"->classContainsPattern["lnum"]], _]];
		FirstCase[
			lnumColumns,
			XMLElement["span", KeyValuePattern["class"->"xlabel"(*"xtr:label"*)], {lnum_}] :> lnum,
			Missing[],
			Infinity
		]
	]

getXMLStrings[xml:XMLElement[_,_,c_]] := Catenate[getXMLStrings/@c]
getXMLStrings[str_?StringQ] := {str}
getXMLStrings[_] := {}

getColumnsTransliteration[cols_] :=
	StringJoin@*getXMLStrings/@Cases[cols, XMLElement[_, KeyValuePattern["class"->classContainsPattern["tlit"|"c"]], _]]

getColumnsTranslation[cols_] :=
	Module[{xtrColumns, cells},
		xtrColumns = Cases[cols, XMLElement[_, KeyValuePattern["class"->classContainsPattern["xtr"]], _]];
		cells = Cases[xtrColumns, XMLElement[_,KeyValuePattern["class"->"cell"],_], Infinity];
		StringJoin@*getXMLStrings/@If[Length[cells] === 0, xtrColumns, cells]
	]

parseRow[row_] :=
	With[{cols = Cases[row,XMLElement["td",_,_],Infinity]},
		{
			getColumnsLineNumber[cols],
			getColumnsTransliteration[cols],
			getColumnsTranslation[cols]
		}
	]

parseHTMLText[text_] :=
	Module[{transliterationXML, rows, parsedRows},
		transliterationXML =
			FirstCase[
				text,
				XMLElement["table", KeyValuePattern["class"->"transliteration"], t_] :> t,
				{},
				Infinity
			];
		rows = Cases[transliterationXML, XMLElement["tr",_,_], Infinity];
		parsedRows = parseRow/@rows;
		parsedRows = If[MissingQ[#[[1]]], Delimiter, #]&/@parsedRows;
		parsedRows //. {
				{Delimiter,r___} :> {r},
				{r___,Delimiter} :> {r},
				{a___,Delimiter,Delimiter,b___} :> {a,Delimiter,b}
			}
	]

oraccObjectHTMLLines[projSpec_, obj_] := oraccObjectParseHTML[oraccObjectHTML[projSpec,obj]]


(* OraccData *)

lineTranslationText[Delimiter, _] := ""
lineTranslationText[line_, True] := line[[1]] <> "\t" <> StringRiffle[line[[3]],"\t"]
lineTranslationText[line_, False] := StringRiffle[line[[3]],"\t"]

lineTransliterationText[Delimiter, _] := ""
lineTransliterationText[line_, True] := line[[1]] <> "\t" <> StringRiffle[line[[2]],"\t"]
lineTransliterationText[line_, False] := StringRiffle[line[[2]],"\t"]

getOraccLinesProperty[lines_,"Lines"] :=
	lines

getOraccLinesProperty[lines_,"Translation"] :=
	StringRiffle[lineTranslationText[#,False]&/@lines,"\n"]
	
getOraccLinesProperty[lines_,"TranslationWithLineNumbers"] :=
	StringRiffle[lineTranslationText[#,True]&/@lines,"\n"]
	
getOraccLinesProperty[lines_,"Transliteration"] :=
	StringRiffle[lineTransliterationText[#,False]&/@lines,"\n"]
	
getOraccLinesProperty[lines_,"TransliterationWithLineNumbers"] :=
	StringRiffle[lineTransliterationText[#,True]&/@lines,"\n"]

getOraccLinesProperty[lines_,props_List] :=
	getOraccLinesProperty[lines,#]&/@props
	
getOraccLinesProperty[lines_,prop_]:=
	Failure["UnknownProperty",<|
		"MessageTemplate"->"Unknown text property `1` encountered.",
		"MessageParameters"->{prop}
	|>]

Options[OraccData] = {
	"OraccBase":>$OraccBase
};

OraccData[project_,text_,prop_,opts:OptionsPattern[]] :=
	Block[{$OraccBase=OptionValue["OraccBase"]},
		getOraccLinesProperty[oraccObjectHTMLLines[project,text],prop]
	]

OraccData[project_,text_,opts:OptionsPattern[]] :=
	OraccData[project,text,"Translation",opts]
	
OraccData[project_,opts:OptionsPattern[]]:=
	oraccProjectCatalog[project]


End[];
EndPackage[];