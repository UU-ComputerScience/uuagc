digraph ndg_A {
attr_Syn_A_outcir [shape=box,label="Syn @A.outcir"]
}

digraph pdg_P {
attr_Inh_tB_incir [shape=box,label="Inh @tB.incir"]
attr_Syn_lhs_outcir [shape=box,label="Syn @lhs.outcir"]
attr_Syn_tB_outcir [shape=box,label="Syn @tB.outcir"]
child_tB [shape=ellipse,label="Child tB"]
rule_rule0 [shape=diamond,label="rule0"]
attr_Syn_lhs_outcir -> rule_rule0 
attr_Syn_tB_outcir -> attr_Inh_tB_incir 
attr_Syn_tB_outcir -> child_tB 
rule_rule0 -> attr_Syn_tB_outcir 
info [shape=box,label="[(tB,B)]"];
}

digraph ndg_B {
attr_Syn_B_outcir [shape=box,label="Syn @B.outcir"]
attr_Inh_B_incir [shape=box,label="Inh @B.incir"]
attr_Syn_B_outcir -> attr_Inh_B_incir 
}

digraph pdg_Q {
attr_Inh_lhs_incir [shape=box,label="Inh @lhs.incir"]
attr_Syn_lhs_outcir [shape=box,label="Syn @lhs.outcir"]
rule_rule1 [shape=diamond,label="rule1"]
attr_Syn_lhs_outcir -> rule_rule1 
rule_rule1 -> attr_Inh_lhs_incir 
info [shape=box,label="[]"];
}

digraph ndg_Root {
attr_Syn_Root_outcir [shape=box,label="Syn @Root.outcir"]
}

digraph pdg_Root {
attr_Syn_lhs_outcir [shape=box,label="Syn @lhs.outcir"]
attr_Syn_tA_outcir [shape=box,label="Syn @tA.outcir"]
child_tA [shape=ellipse,label="Child tA"]
rule_rule2 [shape=diamond,label="rule2"]
attr_Syn_lhs_outcir -> rule_rule2 
attr_Syn_tA_outcir -> child_tA 
rule_rule2 -> attr_Syn_tA_outcir 
info [shape=box,label="[(tA,A)]"];
}
