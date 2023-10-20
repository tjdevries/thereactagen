/** @type {import('tailwindcss').Config} */
module.exports = {
  content: [
    "./lib/**/*.ml",
    "../../lib/**/*.ml",
    "../../../lib/**/*.ml",
    "./bin/**/*.ml",
    "../../bin/**/*.ml",
    "../../../bin/**/*.ml",
  ],
  darkMode: "class",
  theme: {
    extend: {},
  },
  plugins: [],
};
