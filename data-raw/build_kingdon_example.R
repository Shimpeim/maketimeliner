# Builds inst/extdata/kingdon/*.csv (synthetic, illustrative case).
# Run from the package root after devtools::load_all().
nodes <- data.frame(
  id = c("p00","p03","p06","p08",  "s01","s04","s07","s09",  "e02","e05","e08",
         "m01","m04","m07"),
  label = c("Cost indicators\npublished", "Focusing event:\nhospital crisis",
            "Programme\nevaluation", "Media agenda\npeaks",
            "Think-tank\nidea", "Softening up", "Feasibility\nstudy", "Bill adopted",
            "Drafts\nproposal", "Frames the\ncrisis", "Lobbies new\ngovernment",
            "Public mood\nshifts", "Interest-group\ncampaign", "Election:\nturnover"),
  time = c(2000, 2003, 2006, 2008,  2001, 2004, 2007, 2009,  2002, 2005, 2008,
           2001, 2004, 2007),
  stream = c(rep("problem", 4), rep("policy", 4), "policy", "problem", "politics",
             rep("politics", 3)),
  shape = c("circle","diamond","circle","circle",  "circle","circle","circle","rounded",
            "triangle","triangle","triangle",  "circle","circle","diamond"),
  colour = c("#FFFFFF","#FDE2E2","#FFFFFF","#FFFFFF",  "#FFFFFF","#FFFFFF","#FFFFFF","#FFF4C2",
             "#E8F0FE","#E8F0FE","#E8F0FE",  "#FFFFFF","#FFFFFF","#FDE2E2"),
  width = c(14,16,14,14, 14,14,14,26, 15,15,15, 14,14,16),
  height = c(14,16,14,14, 14,14,14,18, 15,15,15, 14,14,16),
  label_position = c(rep("above", 7), "below", rep("above", 6)),
  description = c(
    "Routine indicators show treatment costs rising faster than inflation.",
    "A highly visible crisis draws attention to the problem (focusing event).",
    "Feedback from existing programmes confirms the problem.",
    "The problem reaches the top of the media agenda.",
    "An idea floats in the policy primeval soup.",
    "Policy community discusses and recombines the idea.",
    "Technical feasibility and value acceptability are tested.",
    "Streams are coupled during the policy window; the bill passes.",
    "The entrepreneur drafts a proposal in advance of a window.",
    "The entrepreneur attaches the proposal to the problem framing.",
    "The entrepreneur works the new government's agenda.",
    "National mood begins to favour action.",
    "Organised interests campaign on the issue.",
    "A change of government opens the political stream."),
  stringsAsFactors = FALSE)

ids <- nodes$id
adj <- matrix("0", length(ids), length(ids), dimnames = list(ids, ids))
link <- function(a, b, code) {
  adj[a, b] <<- if (adj[a, b] == "0") code else paste(adj[a, b], code, sep = ";")
}
# 1 issue: problem stream, then crosses to policy at coupling
link("p00","p03","1"); link("p03","p06","1"); link("p06","p08","1"); link("p08","s09","1")
# 2 proposal: policy stream
link("s01","s04","2"); link("s04","s07","2"); link("s07","s09","2")
# 3 entrepreneur: crosses policy -> problem -> politics -> policy
link("e02","e05","3"); link("e05","e08","3"); link("e08","s09","3")
# 4 national mood: politics stream, joins the entrepreneur's node
link("m01","m04","4"); link("m04","m07","4"); link("m07","e08","4"); link("e08","s09","4")

edge_props <- data.frame(
  weight = 1:4,
  label = c("Issue: treatment costs", "Proposal: coverage scheme",
            "Policy entrepreneur", "National mood"),
  colour = c("#D55E00", "#0072B2", "#009E73", "#CC79A7"),
  alpha = c(0.9, 0.9, 0.95, 0.8),
  width = c(4, 4, 3, 3),
  linetype = c("solid", "solid", "solid", "longdash"),
  description = c("Condition defined as a public problem.",
                  "Policy alternative developed in the policy community.",
                  "Actor who couples problems, policies and politics.",
                  "Politics stream: mood, interest groups, government."),
  stringsAsFactors = FALSE)

overlay <- matrix("0", length(ids), length(ids), dimnames = list(ids, ids))
overlay["e08", "p08"] <- "1"; overlay["e08", "s07"] <- "1"
overlay_edge_props <- data.frame(weight = 1, label = "Coupling by entrepreneur",
                                 colour = "#009E73", alpha = 0.8, width = 1.4,
                                 linetype = "dashed", stringsAsFactors = FALSE)
overlay_labels <- matrix("", length(ids), length(ids), dimnames = list(ids, ids))
overlay_labels["e08", "p08"] <- "couples"

streams <- data.frame(
  id = c("problem", "policy", "politics"),
  label = c("Problem\nstream", "Policy\nstream", "Politics\nstream"),
  order = 1:3,
  colour = c("#FBEDE7", "#E7F0FA", "#F3E9F5"),
  alpha = 1, stroke = "#DDDDDD", stroke_width = 1, linetype = "solid",
  height = 150, fontcolour = "#333333",
  description = c("Conditions come to be defined as problems (indicators, focusing events, feedback).",
                  "Ideas and alternatives generated and refined by specialists.",
                  "National mood, organised interests, turnover in government."),
  stringsAsFactors = FALSE)

windows <- data.frame(start = 2007.7, end = 2009.3, label = "Policy window",
                      colour = "#F4C430", alpha = 0.2, stroke = "#C89B00",
                      stroke_width = 1, linetype = "dashed", streams = "",
                      description = "Problems, policies and politics converge; the entrepreneur couples the streams.",
                      stringsAsFactors = FALSE)

legend_shapes <- data.frame(shape = c("diamond", "triangle", "rounded"),
                            label = c("Focusing / political event",
                                      "Entrepreneur action", "Decision"))

spec <- timeline_spec(adj, nodes, streams, edge_props,
                      adj_overlay = overlay, overlay_edge_props = overlay_edge_props,
                      overlay_edge_labels = overlay_labels, windows = windows,
                      legend_node_shape = legend_shapes,
                      settings = list(title = "Multiple Streams: an illustrative case",
                                      axis_label = "Year", label_position = "above",
                                      plot_width = 950))
unlink(list.files("inst/extdata/kingdon", full.names = TRUE))
write_timeline_csvs(spec, "inst/extdata/kingdon", prefix = "kingdon")
