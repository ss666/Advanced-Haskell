import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import Data.IORef
import Parsers


type Memory =String 
type Result =String   

type Calculator =  (Memory,Result)

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup

setup ::Window ->UI()
setup window = do
    return window # set UI.title "calculator"  

    input <-UI.input # set value ""  # set (attr "readonly") "true" # set (attr "style") "width:200px;margin-left:auto; margin-right:auto;"

    buttonEqual <- UI.button # set UI.text "="  
    buttons <- mapM mkButton $ map show [0..9]
    buttonOps <- mapM mkButton ["+","-","x","/","(",")","."]
    buttonSign <-mkButton "+/-"
    buttonCE <- mkButton "CE"
    buttonC <- mkButton "C"
    buttonMA <- mkButton "M+"
    buttonMR <- mkButton "MR"
    buttonMC <- mkButton "MC"

    buttonsRow <- row $map element buttons
    buttonOpsRow <- row $map element buttonOps
    buttonMemoryRow <- row [element buttonMA, element buttonMC, element buttonMR]
    
    getBody window #+ [UI.center #+ [element input, element buttonsRow, element buttonOpsRow, element buttonEqual, element buttonSign, element buttonCE, element buttonC, element buttonMemoryRow]]

    let toAdds = zipWith toAdd buttons $ map show [0..9]
    let toAddOps =zipWith toAdd buttonOps ["+","-","*","/","(",")","."]
    let toClearEntrys = toClearEntry buttonCE
    let toSigns =toSign buttonSign
    let toEvaluates =toEvaluate buttonEqual
    let toClears = toClear buttonC
    let toMemoryAdds = toMemoryAdd buttonMA
    let toMemoryClears = toMemoryClear buttonMC
    let toMemoryRecalls = toMemoryRecall buttonMR
    result <- accumB emptyCalculator $ foldl1 (unionWith const) $ toAdds++toAddOps++[toClearEntrys]++[toSigns]++[toEvaluates]++[toClears]++[toMemoryAdds]++[toMemoryClears]++[toMemoryRecalls] --type Event a = [(Time,a)]
    element input # sink value (fmap snd result)  --fmap ??
    return() -- ????????

emptyCalculator :: Calculator
emptyCalculator = ([],[])


mkButton :: String -> UI Element
mkButton a = UI.button # set UI.text a

toAdd :: Element -> String -> Event(Calculator -> Calculator)
toAdd button content = (add content) <$ UI.click button

toEvaluate :: Element -> Event(Calculator -> Calculator)
toEvaluate button =(evaluate) <$ UI.click button

toSign :: Element -> Event(Calculator -> Calculator)
toSign button =(sign) <$ UI.click button

toClearEntry :: Element -> Event(Calculator -> Calculator)
toClearEntry button =(cclearEntry) <$ UI.click button

toClear :: Element -> Event(Calculator -> Calculator)
toClear button =(cclear) <$ UI.click button

toMemoryAdd :: Element -> Event(Calculator -> Calculator)
toMemoryAdd button =(memoryAdd) <$ UI.click button

toMemoryClear :: Element -> Event(Calculator -> Calculator)
toMemoryClear button =(memoryClear) <$ UI.click button

toMemoryRecall :: Element -> Event(Calculator -> Calculator)
toMemoryRecall button =(memoryRecall) <$ UI.click button


add :: String -> Calculator -> Calculator
add content (memory,result) = (memory, result ++ content)

evaluate :: Calculator -> Calculator
evaluate (memory,result) = (memory,eval result)

sign :: Calculator -> Calculator
sign  (memory,result) = (memory, (negative.eval) result)  

cclearEntry :: Calculator -> Calculator
cclearEntry  (memory,result) = (memory, init result)

cclear :: Calculator -> Calculator
cclear (memory,_) =  (memory,[])

memoryAdd :: Calculator -> Calculator
memoryAdd (_,result) = (eval result, eval result)

memoryClear :: Calculator -> Calculator
memoryClear (_,result) = ([],result)

memoryRecall :: Calculator -> Calculator
memoryRecall (memory, result) = case memory of 
                                    [] -> ([],result)
                                    memory -> (memory,result ++ "(" ++ memory ++ ")")