module NotebookUtils

open Microsoft.DotNet.Interactive.Formatting

module Style =
    /// A CellStyler is a function that takes coordinates in a 2DArray and the value at the given cell
    /// and can return a string that will be used as CSS style for the cell.
    type CellStyler<'a> = int -> int -> 'a -> string

    let objStyler : CellStyler<'a> = fun _ _ _ -> ""
    let stringStyler : CellStyler<string> = fun _ _ _ -> ""

    let intStyler : CellStyler<int> = fun _ _ _ -> ""

    let charStyler : CellStyler<char> = fun _ _ _ -> ""

// Add these helper functions
let getHash (input: 'a) =
    let str = sprintf "%A" input
    let mutable hash = 5381
    for c in str do
        hash <- ((hash <<< 5) + hash) + int c
    hash

let colorForValue (input: 'a) =
    let hash = getHash input
    // Use HSL for better color distribution
    // Map hash to Hue (0-360), keep Saturation and Lightness constant
    let hue = abs (hash % 360)
    sprintf "border-color: hsl(%d, 70%%, 60%%); border-width: 3px" hue

let registerArrayFormatter<'a> () =
    do Formatter.Register<'a[,] * Style.CellStyler<'a>>(
        formatter = (fun (arr, styler) -> 
            let formatCell i j value =
                let style = styler i j value
                let styleAttr = if style = "" then "" else sprintf "style='%s'" style
                sprintf "<td %s>%s</td>" styleAttr (string value)

            let colHeaders = 
                [| 
                    yield "<th></th>"
                    for j in 0..Array2D.length2 arr - 1 -> 
                        sprintf "<th>%d</th>" j 
                |]
                |> String.concat ""
                |> sprintf "<tr>%s</tr>"
                
            let rows = 
                [|
                    yield colHeaders
                    for i in 0..Array2D.length1 arr - 1 ->
                        [|
                            yield sprintf "<th>%d</th>" i
                            for j in 0..Array2D.length2 arr - 1 -> 
                                formatCell i j arr.[i,j]
                        |]
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
                        border: 1px dashed #111;
                    }
                </style>
            </div>
            """ (String.concat "" rows)),
        mimeType = "text/html"
    )



let displayGrid (grid: 'a[,])=
    grid, Style.objStyler

do registerArrayFormatter<int>()
do registerArrayFormatter<float>()
do registerArrayFormatter<char>()
do registerArrayFormatter<string>()
do registerArrayFormatter<bool>()
do registerArrayFormatter<obj>()
