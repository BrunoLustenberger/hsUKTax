{- |
Module : UKIncomeTaxGraphs
Description : Shows a graph of the functions of UKIncomeTax.
Copyright : (c) 2021 Bruno M.S. Lustenberger

Shows the graphs of the relevant functions over an appropriate income range.
The intended range is -10'000 to 160'000. However, plotting explicitely over 
this range takes too long, therefore both axes are shrunk by a factor of 1'000. 
Thus the unit is not 1£ but 1'000 £.

To run this module, you must have installed gnuplot and easyplot.

Usage with ghci:
$ ghci
Prelude> :l UKIncomeTaxGraphs.hs
[1 of 2] Compiling UKIncomeTax      ( UKIncomeTax.hs, interpreted )
[2 of 2] Compiling UKIncomeTaxGraphs ( UKIncomeTaxGraphs.hs, interpreted )
Ok, two modules loaded.
*UKIncomeTaxGraphs> 

The graphs are now in the file graphs.pdf
-}
module UKIncomeTaxGraphs (
      doPlot
    ) where

import Graphics.EasyPlot
import UKIncomeTax

-- helpers to scale the functions

intFuncAsDouble :: (Int -> Int) -> (Double -> Double)
intFuncAsDouble f = fromIntegral . f . round

scaleFunc :: (Double -> Double) -> (Double -> Double)
scaleFunc f = (/1000).f.(*1000)

graphFunc :: (Int -> Int) -> (Double -> Double)
graphFunc = scaleFunc . intFuncAsDouble

-- |Plots the graph
doPlot =
    plot (PDF "graphs.pdf")
        [ Function2D [Title "100%", Color Black] [rg] (\x->x)
        , Function2D [Title "personal allowance", Color Blue] [rg] pa
        , Function2D [Title "taxable income", Color Green] [rg] ti
        , Function2D [Title "income tax", Color Red] [rg] it
        ]
        where   it = graphFunc incomeTax
                ti = graphFunc taxableIncome
                pa = graphFunc personalAllowance
                rg = Range (-10) 160