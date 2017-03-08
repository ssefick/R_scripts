makeNiceNumber = function(num, num.pretty = 1) {
   # Rounding provided by code from Maarten Plieger
   return((round(num/10^(round(log10(num))-1))*(10^(round(log10(num))-1))))
}

createBoxPolygon = function(llcorner, width, height) {
   relativeCoords = data.frame(c(0, 0, width, width, 0), c(0, height, 
height, 0, 0))
   names(relativeCoords) = names(llcorner)
   return(t(apply(relativeCoords, 1, function(x) llcorner + x)))
}

addScaleBar = function(ggplot_obj, spatial_obj, attribute, addParams = 
list()) {
   addParamsDefaults = list(noBins = 5, xname = "x", yname = "y", unit = 
"m", placement = "bottomright",
                            sbLengthPct = 0.3, sbHeightvsWidth = 1/14)
   addParams = modifyList(addParamsDefaults, addParams)

   range_x = max(spatial_obj[[addParams[["xname"]]]]) - 
min(spatial_obj[[addParams[["xname"]]]])
   range_y = max(spatial_obj[[addParams[["yname"]]]]) - 
min(spatial_obj[[addParams[["yname"]]]])
   lengthScalebar = addParams[["sbLengthPct"]] * range_x
   ## OPTION: use pretty() instead
   widthBin = makeNiceNumber(lengthScalebar / addParams[["noBins"]])
   heightBin = lengthScalebar * addParams[["sbHeightvsWidth"]]
   lowerLeftCornerScaleBar = c(x = 
max(spatial_obj[[addParams[["xname"]]]]) - (widthBin * 
addParams[["noBins"]]),
                               y = min(spatial_obj[[addParams[["yname"]]]]))

   scaleBarPolygon = do.call("rbind", lapply(0:(addParams[["noBins"]] - 
1), function(n) {
     dum = data.frame(createBoxPolygon(lowerLeftCornerScaleBar + c((n * 
widthBin), 0), widthBin, heightBin))
     if(!(n + 1) %% 2 == 0) dum$cat = "odd" else dum$cat = "even"
     return(dum)
   }))
   scaleBarPolygon[[attribute]] = min(spatial_obj[[attribute]])
   textScaleBar = data.frame(x = 
lowerLeftCornerScaleBar[[addParams[["xname"]]]] + 
(c(0:(addParams[["noBins"]])) * widthBin),
                             y = 
lowerLeftCornerScaleBar[[addParams[["yname"]]]],
                             label = 
as.character(0:(addParams[["noBins"]]) * widthBin))
   textScaleBar[[attribute]] = min(spatial_obj[[attribute]])

   return(ggplot_obj +
     geom_polygon(data = subset(scaleBarPolygon, cat == "odd"), fill = 
"black", color = "black", legend = FALSE) +
     geom_polygon(data = subset(scaleBarPolygon, cat == "even"), fill = 
"white", color = "black", legend = FALSE) +
     geom_text(aes(label = label), color = "black", size = 6, data = 
textScaleBar, hjust = 0.5, vjust = 1.2, legend = FALSE))
}
