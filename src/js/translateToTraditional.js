import characters from "./characters";

export default function translateToTraditional(text) {
  const textArray = text.split("");
  for (let i = 0; i < text.length; i++) {
    const char = text.charAt(i);
    const traditional = characters[char] && characters[char].traditional;
    if (traditional) {
      textArray[i] = traditional;
    }
  }
  return textArray.join("");
}
