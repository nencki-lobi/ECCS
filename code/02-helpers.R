# Define helpers

ords = 0:179
parts = 0:6

labels_scales = c("Valence", "Arousal", "Anger", "Anxiety", "Compassion", "Guilt", "Hope")
labels_emotion_scales = labels_scales[!(labels_scales %in% c('Valence','Arousal'))]

labels_categories = c("ANG", "ANX", "COM", "GUI", "HOP", "NEU")
colors_categories = c("#E05263", "#659157", "#FEAEA5", "#6C5670", "#46b7d3", "#96949B")

labels_classes = c(labels_categories, "unclassified")
colors_classes = c(colors_categories, "gray85")
names(colors_classes) = labels_classes

code_to_ord = ords
names(code_to_ord) = items$code

ord_to_code = items$code
names(ord_to_code) = ords

ord_to_category = substr(items$code, 1, 3)
names(ord_to_category) = ords

part_to_scale = labels_scales
names(part_to_scale) = parts

# How to use: 
# code_to_ord["GUI2"]
# ord_to_code["0"]
# ord_to_category["0"]

beauty = theme_linedraw() + theme(panel.grid = element_blank(),
                                  strip.background = element_rect(fill = "white", color = "white"),
                                  strip.text = element_text(colour = "black", size = 12),
                                  aspect.ratio = 1)

# Save helpers

if (!dir.exists("./output")) {dir.create("./output")}

helpers = c("ords", "parts", "code_to_ord", "ord_to_code", "ord_to_category", "part_to_scale",
        "labels_scales", "labels_emotion_scales", "labels_categories", "labels_classes",
        "colors_categories", "colors_classes", "beauty")

save(list = helpers, file = "./output/helpers.RData")