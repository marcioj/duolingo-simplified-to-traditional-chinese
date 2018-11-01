import test from "ava";
import translateToTraditional from "./translateToTraditional";

test("translates single character", t => {
  t.is(translateToTraditional("梦"), "夢");
});

test("translates compound character", t => {
  t.is(translateToTraditional("不客气"), "不客氣");
});

test("translates phrase with mixed characters", t => {
  t.is(
    translateToTraditional("不客气 means you're welcome"),
    "不客氣 means you're welcome"
  );
});
