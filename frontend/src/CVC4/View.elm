module CVC4.View exposing (view)

import CVC4.Model as Model
import CVC4.Update as Update
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Util
import ViewUtil


view : Model.Params -> Html Update.ParamMsg
view params =
    div [ class "form-group columns" ] <|
        [ ViewUtil.selectColumn (List.map Model.stringOfLang Model.langs) (\str -> Update.Lang <| Util.unwrap <| Model.langOfString str) "lang"
        , ViewUtil.selectColumn (List.map Model.stringOfOutputLang Model.outputLangs) (\str -> Update.OutputLang <| Util.unwrap <| Model.outputLangOfString str) "output lang"
        , ViewUtil.inputNumberColumn Update.Verbosity "verbosity"
        , ViewUtil.checkboxColumn Update.Stats "stats"
        , ViewUtil.checkboxColumn Update.StrictParsing "strict parsing"
        , ViewUtil.checkboxColumn Update.CpuTime "cpu time"
        , ViewUtil.checkboxColumn Update.HardLimit "hard limit"
        , ViewUtil.checkboxColumn Update.Incremental "incremental"
        , ViewUtil.checkboxColumn Update.ProduceAssertions "produce assertions"
        , ViewUtil.checkboxColumn Update.ProduceModels "produce models"
        , ViewUtil.checkboxNumberColumn Update.Seed "seed" params.seed
        , ViewUtil.checkboxNumberColumn Update.ResourceLimitPer "resource limit per" params.resourceLimitPer
        , ViewUtil.checkboxNumberColumn Update.ResourceLimit "resource limit" params.resourceLimit
        , ViewUtil.checkboxNumberColumn Update.TimeLimitPer "time limit per" params.timeLimitPer
        , ViewUtil.checkboxNumberColumn Update.TimeLimit "time limit" params.timeLimit
        , ViewUtil.registerParamsColumn Update.AddOption Update.RemoveOption Update.InputtingOption "others:" params.others
        ]
