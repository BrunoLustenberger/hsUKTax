{- Shows the graphs of the relevant functions over income range -10'000 to 160'000.
   Plotting explicitely over this range takes too long, therefore both axes are
   shrunk by a factor of 1'000. Thus the unit is not 1£ but 1'000 £.

   Usage with ghci:
    $ ghci
    Prelude> :load UKIncomeTaxGraphs
    [1 of 2] Compiling UKIncomeTax      ( UKIncomeTax.hs, interpreted )
    [2 of 2] Compiling Main             ( UKIncomeTaxGraphs.hs, interpreted )
    Ok, two modules loaded.
    *Main> doPlot

    The graphs are now in the file graphs.pdf
-}

import Graphics.EasyPlot
import UKIncomeTax

-- helpers to scale the functions

intFuncAsDouble :: (Int -> Int) -> (Double -> Double)
intFuncAsDouble f = fromIntegral . f . round

scaleFunc :: (Double -> Double) -> (Double -> Double)
scaleFunc f = (/1000).f.(*1000)

graphFunc :: (Int -> Int) -> (Double -> Double)
graphFunc = scaleFunc . intFuncAsDouble

-- plotting

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