Tools.Assign <-
function(var, env = .GlobalEnv)
{
    name = deparse(substitute(var))
    
    assign(name, var, envir = env)
}
