# Define helpers

ords = 0:179
parts = 0:6
labels_scales = c("Valence", "Arousal", "Anger", "Anxiety", "Compassion", "Guilt", "Hope")
labels_categories = c("ANG", "ANX", "COM", "GUI", "HOP", "NEU")
colors_categories = c("#E05263", "#659157", "#FEAEA5", "#6C5670", "#69A2B0", "#96949B")

code_to_ord = ords
names(code_to_ord) = items$code

ord_to_code = items$code
names(ord_to_code) = ords

ord_to_category = substr(items$code, 1, 3)
names(ord_to_category) = ords

part_to_scale = labels_scales
names(part_to_scale) = parts

categories = ord_to_category[as.character(ords)]

# How to use: 
# code_to_ord["GUI2"]
# ord_to_code["0"]
# ord_to_category["0"]