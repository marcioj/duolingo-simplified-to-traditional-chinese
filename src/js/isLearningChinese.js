export default function isLearningChinese() {
  const state = JSON.parse(localStorage.getItem("duo.state"));
  return state.user.courseId.includes("ZH-CN_EN");
}
