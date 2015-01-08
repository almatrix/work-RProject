cols = 110
rows = 99
ncls.hour = 6
ncls.grid = 20
ncls.ia.h.g = 10
ncls.ia.h.g.w = 10
job.id = 26
job.group.id = 2
regression.formula = paste("cate_l1 ~", "ugrid.id.cid", 
						"+ hour.cid",
						"+ last.cate_l1",
						"+ isweekend", 

						"+ hour_grid.cid",
						"+ hour_grid_weekday.cid",

                        "- 1")