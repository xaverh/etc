module.exports = {
	extends: ['standard', 'prettier', 'prettier/standard'],
	env: {
		es6: true,
		node: true,
	},
	rules: {
		'no-console': process.env.NODE_ENV === 'production' ? 'warn' : 'off',
		'no-debugger': process.env.NODE_ENV === 'production' ? 'warn' : 'off',
	},
	parserOptions: {
		ecmaVersion: 2020,
	},
}

/*
{
  "extends": [
    "standard",
    "plugin:@typescript-eslint/recommended",
    "plugin:flowtype/recommended",
    "plugin:react/recommended",
    "plugin:unicorn/recommended",
    "plugin:vue/recommended",
    "prettier",
    "prettier/@typescript-eslint",
    "prettier/babel",
    "prettier/flowtype",
    "prettier/react",
    "prettier/standard",
    "prettier/unicorn",
    "prettier/vue"
  ],
  "plugins": [
    "@typescript-eslint",
    "babel",
    "flowtype",
    "react",
    "standard",
    "unicorn",
    "vue"
  ],
  "parserOptions": {
    "sourceType": "module",
    "ecmaFeatures": {
      "jsx": true
    }
  },
  "env": {
    "es6": true,
    "node": true
  }
}
*/

// module.exports = {
// 	root: true,
// 	env: {
// 		node: true,
// 	},
// 	extends: [
// 		'plugin:vue/essential',
// 		'@vue/standard',
// 		'@vue/typescript/recommended',
// 		'@vue/prettier',
// 		'@vue/prettier/@typescript-eslint',
// 	],
// 	parserOptions: {
// 		ecmaVersion: 2020,
// 	},
// }
