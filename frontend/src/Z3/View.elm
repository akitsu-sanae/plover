module Z3.View exposing (view)

import Html exposing (Html, br, div, input, option, select, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Util
import ViewUtil
import Z3.Model as Model
import Z3.Update as Update


view : Model.Params -> Html Update.ParamMsg
view params =
    div [ class "form-group columns" ] <|
        [ ViewUtil.selectColumn (List.map Model.stringOfFormat Model.formats) (\str -> Update.Format <| Util.unwrap <| Model.formatOfString str) "format"
        , ViewUtil.checkboxColumn Update.DisplayGlobalParams "display global parameters"
        , ViewUtil.checkboxColumn Update.DisplayGlobalParamDescs "display global parameter descriptions"
        , ViewUtil.checkboxColumn Update.DisplayStatistics "display statistics"
        , ViewUtil.checkboxColumn Update.DisplayWarnings "display warnings"
        , ViewUtil.checkboxNumberColumn Update.Timeout "timeout" params.limit.timeout
        , ViewUtil.checkboxNumberColumn Update.SoftTimeout "soft timeout" params.limit.softTimeout
        , ViewUtil.checkboxNumberColumn Update.Memory "memory" params.limit.memory
        ]
