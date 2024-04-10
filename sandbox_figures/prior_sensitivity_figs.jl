using DelimitedFiles
using DataFrames
using Plots


# Access the data
cd("/home/sebastiand/Dropbox/projects_WORKING/BETS_prior_sensitivity/BETS_prior_sensitivity_ms/sandbox_figures")

# Your code here
# Specify the path to your tab-delimited file
file_path = "prior_predictive_simulations/cholera_aln_prior_uniformPhi.log"

P, H = readdlm(file_path, '\t', header = true, comments = true, comment_char = '#')


df = DataFrame(P, Symbol.(vec(H)))

names(df)

plot(df[:, "constant.popSize"], df[:, :treeLength], xlabel = "Column 1", ylabel = "Column 2", legend = false, seriestype = :scatter)


