(* Mathematica Package *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)

(* :Title: USAInterstateHighwaySystem *)
(* :Context: USAInterstateHighwaySystem` *)
(* :Author: Anton Antonov *)
(* :Date: 2022-11-15 *)

(* :Package Version: 0.1 *)
(* :Mathematica Version: 13.1 *)
(* :Copyright: (c) 2022 Anton Antonov *)
(* :Keywords: *)
(* :Discussion: *)

BeginPackage["USAInterstateHighwaySystem`"];

MakeRouteDataObjects::usage = "MakeRouteDataObjects[fileURL, opts___]";

MakeRoutesGraph::usage = "MakeRoutesGraph[aDataObjects_Association, opts___]";

ExportRoutesData::usage = "ExportRoutesData[irName_String, aDataObjects_Association, grIHighwaysReduced_Graph, grIHighways_Graph]";

PacletInstall["AntonAntonov/MonadicGeometricNearestNeighbors", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/DataReshapers", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/OutlierIdentifiers", AllowVersionUpdate -> False];
PacletInstall["AntonAntonov/SSparseMatrix", AllowVersionUpdate -> False];

Begin["`Private`"];

Needs["AntonAntonov`MonadicGeometricNearestNeighbors`"];
Needs["AntonAntonov`DataReshapers`"];
Needs["AntonAntonov`OutlierIdentifiers`"];
Needs["AntonAntonov`SSparseMatrix`"];

(*******************************************************)
(* FindSimilarGeoLines                                 *)
(*******************************************************)

Clear[FindSimilarGeoLines];
FindSimilarGeoLines[id_String, aGeoLines_?AssociationQ, maxDistance_?NumberQ] :=
    FindSimilarGeoLines[aGeoLines[id], aGeoLines, maxDistance] /; KeyExistsQ[aGeoLines, id];

FindSimilarGeoLines[geoLine : {{_?NumberQ, _?NumberQ} ..}, aGeoLines_?AssociationQ, maxDistance_?NumberQ] :=
    Block[{aCandidates0, aCandidates1},
      aCandidates0 = Select[aGeoLines, Length[#] == Length[geoLine] &];
      aCandidates1 =
          Association@
              KeyValueMap[
                Function[{k, v},
                  k -> MapThread[EuclideanDistance, {Sort@geoLine, Sort@v}]],
                aCandidates0];
      aCandidates1 = Select[aCandidates1, Max[#] <= maxDistance &];
      Keys[aCandidates1]
    ];
FindSimilarGeoLines[___] := $Failed;


(*******************************************************)
(* SameGeoLinesQ                                       *)
(*******************************************************)

Clear[SameGeoLinesQ];
SameGeoLinesQ[id1_String, id2_String] :=
    KeyExistsQ[aSimilar, id1] && MemberQ[aSimilar[id1], id2];

(*
See : https://en.wikipedia.org/wiki/Interstate_Highway_System
kmlURL = "https://en.wikipedia.org/w/index.php?title=Template:Attached_KML/Interstate_Highway_System&action=raw";
*)


(*******************************************************)
(* MakeRouteDataObjects                                     *)
(*******************************************************)
fileURL = "./Wikipedia_Interstate_Highway_System.kml";

Clear[MakeRouteDataObjects];

Options[MakeRouteDataObjects] = {
  "MaxRoutePointSimilarityDistance" -> 0.002
};

MakeRouteDataObjects[fileURL_String : fileURL] :=
    Block[{kmlObj,
      lsGeoLines, lsGeoLines2, lsGeoPointAssociations, lsGeoPointAssociations2, lsUniqueGeoLineIDs,
      aGeoLines, aGeoPoints, aSimilar, aGeoLines2, aGeoPoints2, aHWPoints2,
      gnnHWObj2, gnnAllObj2},

      (*-----------------------------------------------------*)
      (* Import data                                         *)
      (*-----------------------------------------------------*)

      Print["Import KML USA highways data:"];
      Print[AbsoluteTiming[
        kmlObj = Import[fileURL, "Data"];
      ]];
      Print["\t...DONE"];

      lsGeoLines = Cases[kmlObj[[1]], Line[__], Infinity];
      lsGeoLines2 = #[[1, 1]] & /@ lsGeoLines;

      aGeoLines = ToAutomaticKeysAssociation[lsGeoLines2];

      (*-----------------------------------------------------*)
      (* Geo point associations                              *)
      (*-----------------------------------------------------*)

      Print["Geo point associations:"];
      Print[AbsoluteTiming[
        lsGeoPointAssociations =
            KeyValueMap[
              Function[{k, v},
                Thread[Rule[
                  Map[k <> "." <> (ToString[#] /. {"1" -> "Start", ToString[Length[v]] -> "End"}) &, Range[Length[v]]], v]]
              ],
              aGeoLines
            ];
      ]];

      aGeoPoints = Association@Flatten[lsGeoPointAssociations];

      (*-----------------------------------------------------*)
      (* Systematic similarity search                        *)
      (*-----------------------------------------------------*)

      Print["Systematic similarity search:"];
      Print[AbsoluteTiming[
        aSimilar =
            Association[
              Map[# ->
                  Complement[FindSimilarGeoLines[#, aGeoLines, 0.002], {#}] &,
                Keys[aGeoLines]]];
        aSimilar = Select[aSimilar, Length[#] > 0 &];
      ]];

      Print["Length[aSimilar] : ", Length[aSimilar]];

      Print[AbsoluteTiming[
        lsUniqueGeoLineIDs =
            Union[Keys[aGeoLines], SameTest -> (SameGeoLinesQ[#1, #2] &)];
      ]];

      Print["Length[lsUniqueGeoLineIDs] : ", Length[lsUniqueGeoLineIDs]];

      (*-----------------------------------------------------*)
      (* Unionized set of points                             *)
      (*-----------------------------------------------------*)

      aGeoLines2 = KeyTake[aGeoLines, lsUniqueGeoLineIDs];
      Print["Length[aGeoLines2] : ", Length[aGeoLines2]];

      Print[AbsoluteTiming[
        lsGeoPointAssociations2 =
            KeyValueMap[
              Function[{k, v},
                Thread[Rule[
                  Map[k <>
                      "." <> (ToString[#] /. {"1" -> "Start",
                    ToString[Length[v]] -> "End"}) &, Range[Length[v]]],
                  v]]], aGeoLines2];
      ]];

      (*lsGeoPointAssociations2[[12 ;; 15]]*)

      aGeoPoints2 = Association@Flatten[lsGeoPointAssociations2];

      Print["Length[aGeoPoints2] : ", Length[aGeoPoints2]];

      (*-----------------------------------------------------*)
      (* GNNMon objects (redo)                               *)
      (*-----------------------------------------------------*)

      aHWPoints2 =
          Association[
            Flatten[KeyValueMap[{#1 <> ".Start" -> First[#2], #1 <> ".End" -> Last[#2]} &, aGeoLines2]]];
      Print["Length[aHWPoints2] : ", Length[aHWPoints2]];

      Print[AbsoluteTiming[
        gnnHWObj2 =
            GNNMonUnit[aHWPoints2]\[DoubleLongRightArrow]
                GNNMonMakeNearestFunction[DistanceFunction -> EuclideanDistance]\[DoubleLongRightArrow]
                GNNMonComputeThresholds[10, "AggregationFunction" -> Mean, "OutlierIdentifier" -> QuartileIdentifierParameters];
      ]];


      Print[AbsoluteTiming[
        gnnAllObj2 =
            GNNMonUnit[aGeoPoints2]\[DoubleLongRightArrow]
                GNNMonMakeNearestFunction[DistanceFunction -> EuclideanDistance]\[DoubleLongRightArrow]
                GNNMonComputeThresholds[10, "AggregationFunction" -> Mean, "OutlierIdentifier" -> QuartileIdentifierParameters];
      ]];

      (*-----------------------------------------------------*)
      (* Make connected-routes graph                         *)
      (*-----------------------------------------------------*)

      <|
        "GeoLines" -> aGeoLines2,
        "GeoPoints" -> aGeoPoints2,
        "HWPoints" -> aHWPoints2,
        "GeoPointAssociations" -> lsGeoPointAssociations2,
        "gnnHWObj" -> gnnHWObj2,
        "gnnAllObj" -> gnnAllObj2
      |>
    ];


(*******************************************************)
(* MakeRoutesGraph                                     *)
(*******************************************************)

Clear[MakeRoutesGraph];

Options[MakeRoutesGraph] = { "Reduced" -> True};

MakeRoutesGraph[aDataObjects_Association, opts : OptionsPattern[] ] :=
    Block[{
      gnnObj2, gnnAllObj2, aGeoPoints2, aGeoLines2,
      matAdj, aRows, aRows2, aRows3, lsGeoPointAssociations2,
      lsConnectEdges, lsEdgesReduced, lsAllEdgesReduced, lsEdges, lsAllEdges,
      grIHighwaysReduced, grIHighways
    },

      gnnObj2 = aDataObjects["gnnHWObj"];
      gnnAllObj2 = aDataObjects["gnnAllObj"];
      aGeoPoints2 = aDataObjects["GeoPoints"];
      aGeoLines2 = aDataObjects["GeoLines"];
      lsGeoPointAssociations2 = aDataObjects["GeoPointAssociations"];

      matAdj =
          gnnObj2\[DoubleLongRightArrow]
              GNNMonComputeAdjacencyMatrix[8, "ImplicitValue" -> Infinity]\[DoubleLongRightArrow]
              GNNMonTakeValue;

      aRows = RowAssociations[matAdj];

      aRows2 = Association@KeyValueMap[#1 -> KeyDrop[#2, #1] &, aRows];

      aRows3 = Map[TakeSmallest[#, UpTo[5]] &, aRows2];

      lsConnectEdges =
          Union@Flatten@
              KeyValueMap[
                Function[{k, v},
                  UndirectedEdge @@@ Map[Sort[{k, #}] &, Keys[v]]], aRows3];

      lsConnectEdges =
          Map[# -> EuclideanDistance[aGeoPoints2[#[[1]]], aGeoPoints2[#[[2]]]] &, lsConnectEdges];

      Print["Length[lsConnectEdges] : ", Length[lsConnectEdges]];

      If[ TrueQ[OptionValue[MakeRoutesGraph, "Reduced"]],

        Print["Make reduced graph:"];
        lsEdgesReduced =
            Flatten@Map[
              UndirectedEdge[#[[1]], #[[2]]] ->
                  EuclideanDistance[aGeoPoints2[#[[1]]], aGeoPoints2[#[[2]]]] &@
                  Keys[#][[{1, -1}]] &, lsGeoPointAssociations2];

        Print["Length[lsEdgesReduced] : ", Length[lsEdgesReduced]];

        lsAllEdgesReduced = Join[lsEdgesReduced, lsConnectEdges];
        Print[AbsoluteTiming[
          grIHighwaysReduced = Graph[Keys@lsAllEdgesReduced, EdgeWeight -> lsAllEdgesReduced];
        ]];

        Print["WeightedGraphQ[grIHighwaysReduced] : ", WeightedGraphQ[grIHighwaysReduced]];

        grIHighwaysReduced,
        (*ELSE*)

        lsEdges =
            Flatten@Map[
              UndirectedEdge[#[[1]], #[[2]]] ->
                  EuclideanDistance[aGeoPoints2[#[[1]]],
                    aGeoPoints2[#[[2]]]] & /@ Partition[Keys[#], 2, 1] &,
              lsGeoPointAssociations2];
        Print["Length[lsEdges] : ", Length[lsEdges]];

        lsAllEdges = Join[lsEdges, lsConnectEdges];
        Print["Length[lsAllEdges] : ", Length[lsAllEdges]];

        lsAllEdges = Join[lsEdges, lsConnectEdges];
        Print["Make full graph:"];
        Print[AbsoluteTiming[
          grIHighways = Graph[Keys@lsAllEdges, EdgeWeight -> lsAllEdges];
        ]];

        Print["WeightedGraphQ[grIHighways] : ", WeightedGraphQ[grIHighways]];

        grIHighways
      ]
    ];


(*******************************************************)
(* MakeRoutesGraph                                     *)
(*******************************************************)

Clear[ExportRoutesData];
Options[ExportRoutesData];
ExportRoutesData[
  dirName_String,
  aDataObjects_Association,
  grIHighwaysReduced_Graph,
  grIHighways_Graph] :=
    Block[{dsExport, dsExport2, lsAllEdgesReduced, lsAllEdges},

      (*Reduced*)
      lsAllEdgesReduced = Map[# -> PropertyValue[{grIHighwaysReduced, #}, EdgeWeight] &, EdgeList[grIHighwaysReduced]];

      dsExport = Dataset[Map[Append[List @@ #[[1]], #[[2]]] &, lsAllEdgesReduced]];

      dsExport2 = (Association["Node1" -> #1[[1]], "Node2" -> #1[[2]], "Weight" -> #1[[3]]] &) /@ dsExport;

      Print["Dimensions[dsExport2] : ", Dimensions[dsExport2]];

      Print[Export[FileNameJoin[{dirName, "usa-highways-reduced-graph-edges.csv"}], dsExport2]];

      (*Full*)

      lsAllEdges = Map[# -> PropertyValue[{grIHighways, #}, EdgeWeight] &, EdgeList[grIHighways]];

      dsExport = Dataset[Map[Append[List @@ #[[1]], #[[2]]] &, lsAllEdges]];
      dsExport2 = (Association["Node1" -> #1[[1]], "Node2" -> #1[[2]], "Weight" -> #1[[3]]] &) /@ dsExport;

      Print["Dimensions[dsExport2] : ", Dimensions[dsExport2]];

      Print[Export[FileNameJoin[{dirName, "usa-highways-graph-edges.csv"}], dsExport2]];

      (*Geo-points*)

      dsExport =
          Dataset[Flatten /@ (List @@@ Normal[aDataObjects["GeoPoints"]])][All, AssociationThread[{"Node", "Lat", "Lon"}, #] &];

      dsExport2 =
          dsExport[All,
            Append[#, "Group" -> StringRiffle[StringSplit[#Node, "."][[1 ;; 2]], "."]] &];

      dsExport2 =
          dsExport2[All,
            Append[#, "IndexInGroup" -> ToExpression[StringSplit[#Node, "."][[-1]] /. {"Start" -> 1, "End" -> -1}]] &];

      Print["Dimensions[dsExport2] : ", Dimensions[dsExport2]];

      Print[
        Export[
          FileNameJoin[{dirName, "usa-highways-graph-vertex-coordinates.csv"}],
          Prepend[Rest[Normal[dsExport2[Values]]], Normal@Keys[dsExport2[[1]]]]
        ]
      ];
    ];

End[]; (* `Private` *)

EndPackage[]