#pragma once

// ANCHOR: plain_class_forward_declaration
namespace rust { class ScoreAdjustment; }

int adjusted_score(const rust::ScoreAdjustment& adjustment, int base);
// ANCHOR_END: plain_class_forward_declaration
