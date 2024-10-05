.__C__linreg <-
new("refClassRepresentation", fieldClasses = list(formula = "formula", 
    data = "data.frame", data_str = "character", formula_str = "character", 
    X = "matrix", y = "numeric", Q = "matrix", R = "matrix", 
    coefficients = "matrix", residuals = "numeric", y_hat = "matrix", 
    std_error = "numeric", dof = "numeric", std_dev = "numeric", 
    var_beta = "matrix", t_value = "matrix", p_value = "matrix", 
    residual_std_error = "numeric", summary_output = "matrix"), 
    refSuperClasses = "envRefClass", slots = list(.xData = structure("environment", package = "methods")), 
    contains = list(envRefClass = new("SClassExtension", subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("envRefClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            class(from) <- "envRefClass"
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            for (what in ".xData") methods::slot(from, what) <- methods::slot(value, 
                what)
            from
        }, simple = TRUE, by = character(0), dataPart = FALSE, 
        distance = 1), .environment = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure(".environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            {
                class(from) <- ".environment"
                from
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- methods::as(from, "envRefClass", TRUE)
            methods::as(.value, ".environment") <- value
            value <- .value
            {
                for (what in ".xData") methods::slot(from, what) <- methods::slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), refClass = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("refClass", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- methods::as(from, "envRefClass", TRUE)
            methods::as(.value, "refClass") <- value
            value <- .value
            {
                for (what in ".xData") methods::slot(from, what) <- methods::slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 2), environment = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("environment", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- as(from, "envRefClass", strict = strict)
            {
                from <- as(from, ".environment", strict = strict)
                from@.xData
            }
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- methods::as(from, "envRefClass", TRUE)
            methods::as(.value, "environment") <- value
            value <- .value
            {
                for (what in ".xData") methods::slot(from, what) <- methods::slot(value, 
                  what)
                from
            }
        }, simple = FALSE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3), refObject = new("SClassExtension", 
        subClass = structure("linreg", package = ".GlobalEnv"), 
        superClass = structure("refObject", package = "methods"), 
        package = ".GlobalEnv", coerce = function (from, strict = TRUE) 
        {
            from <- {
                class(from) <- "envRefClass"
                from
            }
            from
        }, test = function (object) 
        TRUE, replace = function (from, to, value) 
        {
            .value <- methods::as(from, "envRefClass", TRUE)
            methods::as(.value, "refObject") <- value
            value <- .value
            {
                for (what in ".xData") methods::slot(from, what) <- methods::slot(value, 
                  what)
                from
            }
        }, simple = TRUE, by = structure("envRefClass", package = "methods"), 
        dataPart = FALSE, distance = 3)), virtual = FALSE, 
    validity = NULL, access = list(), className = structure("linreg", package = ".GlobalEnv"), 
    package = ".GlobalEnv", subclasses = list(), 
    sealed = FALSE)
.__global__ <-
c("formula", "data", "data_str", "formula_str", "X", "y", "Q", 
"R", "coefficients", "residuals", "y_hat", "std_error", "dof", 
"std_dev", "var_beta", "t_value", "p_value", "residual_std_error", 
"summary_output", "plot", "summary", "resid", "pred", "print", 
"initialize", "field", "trace", "getRefClass", "initFields", 
"copy", "callSuper", ".objectPackage", "export", "untrace", "getClass", 
"show", "usingMethods", ".objectParent", "import", ".self")
.requireCachedGenerics <-
structure(list("$", "$<-"), package = c("base", "base"))
