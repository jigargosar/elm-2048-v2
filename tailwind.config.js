/** @type {import('tailwindcss').Config} */

module.exports = {
  content: ['./src/*.elm', './index.html'],
  theme: {
    extend: {
      colors: {
        dark1: "hsl(225deg 6% 13%)",
      },
    },
  },
  plugins: [],
}
