// From:
// https://rosettacode.org/wiki/Pythagoras_tree#F.23

module fsharp_fable_pythagorus_tree

open System
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

open HtmlUtil

type Point = { x:float; y:float }
type Line = { left : Point; right : Point }

let svg_square x1 y1 x2 y2 x3 y3 x4 y4 =
    (s=?"polygon") [ "points" => sprintf "%i %i %i %i %i %i %i %i" (int x1) (int y1) (int x2) (int y2) (int x3) (int y3) (int x4) (int y4) ] []

type Square = {
    topLeft : Point;
    topRight : Point;
    bottomRight : Point;
    bottomLeft : Point
}

let sprout (lines, leaves) line c2 sc =
    let dx = line.right.x - line.left.x
    let dy = line.left.y - line.right.y
    let line2 = {
        left = { x = line.left.x - dy; y = line.left.y - dx };
        right = { x = line.right.x - dy ; y = line.right.y - dx }
    }
    let triangleTop = {
        x = line2.left.x + dx * c2 - dy * sc;
        y = line2.left.y - dy * c2 - dx * sc
    }
    let leftLine = { left = line2.left; right = triangleTop }
    let rightLine = { left = triangleTop; right = line2.right }
    let newLines = rightLine :: leftLine :: lines
    let newLeaves = rightLine :: leftLine :: leaves
    (newLines, newLeaves)
 
let draw_square line =
    let dx = line.right.x - line.left.x
    let dy = line.left.y - line.right.y
    svg_square line.left.x line.left.y line.right.x line.right.y
               (line.right.x - dy) (line.right.y - dx) (line.left.x - dy) (line.left.y - dx)

let rec generate lines leaves c2 sc = function
| 0 -> lines
| n ->
    let (newLines, newLeaves) = List.foldBack (fun elem acc -> (sprout acc elem c2 sc)) leaves (lines, [])
    generate newLines newLeaves c2 sc (n-1)

let renderLines lines =
    (s=?"svg") ["width" => 600; "height" => 600] (List.map draw_square lines)

let iters = document.getElementById("iterations") :?> HTMLInputElement
let angle = document.getElementById("angle") :?> HTMLInputElement

let init() =
    let output = document.getElementById("output")

    let depth = (int iters.value)
    let theta = (float angle.value) * Math.PI / 180.0
    let c2 = Math.Cos(theta) * Math.Cos(theta)
    let sc = Math.Sin(theta) * Math.Cos(theta)
    let startingLine = [{ left = { x = 275.; y = 500. }; right = { x = 375.; y = 500. } }]
    let lines = generate startingLine startingLine c2 sc depth
    renderLines (List.rev lines) |> renderTo output

iters.addEventListener_input(fun _ -> init(); box())
angle.addEventListener_input(fun _ -> init(); box())

init()