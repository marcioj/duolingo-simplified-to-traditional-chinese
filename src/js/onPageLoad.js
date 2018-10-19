export default function onPageLoad(cb) {
  if (document.readyState !== "complete") {
    setTimeout(() => onPageLoad(cb), 100);
  } else {
    cb();
  }
}
