cols = 40
rows = 36
ncls.hour = 6
ncls.grid = 6 

ncls.ia.h.g.l=10
ncls.ia.h.g.l.w=10
ncls.ia.g.w=10
ncls.ia.h.l=10

job.id = 33
job.group.id = 32

regression.formula = paste("cate_l1 ~", "ugrid.id.cid", 
						"+ hour.cid",
						
						"+ hour_grid_last.cid",
						"+ hour_grid_last_weekday.cid",
						"+ last.cate_l1",

                        "- 1")
#regression.formula = paste("cate_l1 ~", "ugrid.id.cid", 
#						"+ hour.cid",
#
#						"+ last.cate_l1",
#						"+ isweekend", 
#
#						"+ hour_grid.cid",
#						"+ hour_grid_weekday.cid",
#
#                        "- 1")
