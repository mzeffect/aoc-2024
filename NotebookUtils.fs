module NotebookUtils

open Microsoft.DotNet.Interactive.Formatting

type CellStyler = int -> int -> char -> string

let defaultStyler : CellStyler = fun _ _ _ -> ""

// Add these helper functions
let getHash (input: 'a) =
    let str = sprintf "%A" input
    let mutable hash = 5381
    for c in str do
        hash <- ((hash <<< 5) + hash) + int c
    hash

let getColorForValue (input: 'a) =
    let hash = getHash input
    // Use HSL for better color distribution
    // Map hash to Hue (0-360), keep Saturation and Lightness constant
    let hue = abs (hash % 360)
    sprintf "border-color: hsl(%d, 70%%, 60%%); border-width: 3px" hue

// Then register formatter
do Formatter.Register<char[,] * CellStyler>(
    formatter = (fun (arr, styler) -> 
        let colHeaders = 
            [| yield "<th></th>"
               for j in 0..Array2D.length2 arr - 1 -> 
                    sprintf "<th>%d</th>" j |]
            |> String.concat ""
            |> sprintf "<tr>%s</tr>"
            
        let rows = 
            [| yield colHeaders
               for i in 0..Array2D.length1 arr - 1 ->
                    [| yield sprintf "<th>%d</th>" i
                       for j in 0..Array2D.length2 arr - 1 -> 
                            sprintf "<td style='%s'>%A</td>" (styler i j arr.[i,j]) arr.[i,j] |]
                    |> String.concat "" 
                    |> sprintf "<tr>%s</tr>"
            |]
        
        sprintf """
        <div style='font-family: monospace'>
            <table style='border-collapse: collapse'>
                %s
            </table>
            <style>
                td, th { 
                    width: 25px;
                    height: 25px;
                    text-align: center;
                    border: 1px solid #bbb;
                    padding: 4px;
                }
                th {
                    font-weight: normal;
                    border: 1px dashed #aaa;
                }
            </style>
        </div>
        """ (String.concat "" rows)),
    mimeType = "text/html"
)
