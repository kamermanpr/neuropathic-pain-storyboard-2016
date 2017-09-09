# Set additional directories 'make' must search for dependencies in
VPATH = rscripts data figures

# Create dummy targets to ensure all intermediate targets are 'made'
.PHONY: all

STORYBOARD = 	index.html
ACCESSORYFILES = 	burden_plot.RData conditions_plot.RData access_plot.RData \
					prevalence_plot.RData treatment_plot.RData

all: $(STORYBOARD) $(ACCESSORYFILES)


# Generate index.html
#####################
index.html: 	index.Rmd burden_plot.RData conditions_plot.RData \
				access_plot.RData prevalence_plot.RData treatment_plot.RData
	Rscript -e "rmarkdown::render(input = '$<')"

# Generate accessory-files
##########################
data/burden_plot.RData: burden.R
	Rscript "$<"
data/access_plot.RData: access.R
	Rscript "$<"
data/conditions_plot.RData: conditions.R
	Rscript "$<"
data/prevalence_plot.RData: prevalence.R
	Rscript "$<"
data/treatment_plot.RData: treatment.R
	Rscript "$<"
