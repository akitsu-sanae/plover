module CVC4.Update exposing (ParamMsg(..), update)

import CVC4.Model as Model


type ParamMsg
    = Lang Model.Lang
    | OutputLang Model.OutputLang
    | Verbosity Int
    | Stats
    | Seed (Maybe Int)
    | StrictParsing
    | CpuTime
    | HardLimit
    | Incremental
    | ProduceAssertions
    | ProduceModels
    | ResourceLimitPer (Maybe Int)
    | ResourceLimit (Maybe Int)
    | TimeLimitPer (Maybe Int)
    | TimeLimit (Maybe Int)


update : ParamMsg -> Model.Params -> Model.Params
update msg params =
    case msg of
        Lang lang ->
            { params | lang = lang }

        OutputLang outputLang ->
            { params | outputLang = outputLang }

        Verbosity n ->
            { params | verbosity = n }

        Stats ->
            { params | stats = not params.stats }

        Seed n ->
            { params | seed = n }

        StrictParsing ->
            { params | strictParsing = not params.strictParsing }

        CpuTime ->
            { params | cpuTime = not params.cpuTime }

        HardLimit ->
            { params | cpuTime = not params.hardLimit }

        Incremental ->
            { params | incremental = not params.incremental }

        ProduceAssertions ->
            { params | produceAssertions = not params.produceAssertions }

        ProduceModels ->
            { params | produceModels = not params.produceModels }

        ResourceLimitPer limit ->
            { params | resourceLimitPer = limit }

        ResourceLimit limit ->
            { params | resourceLimit = limit }

        TimeLimitPer limit ->
            { params | timeLimitPer = limit }

        TimeLimit limit ->
            { params | timeLimit = limit }
