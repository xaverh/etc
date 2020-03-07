module.exports = {
	tabWidth: 4,
	semi: false,
	singleQuote: true,
	useTabs: true,
	overrides: [
		{
			files: ['*.md', '*.markdown'],
			options: {
				tabWidth: 2,
				useTabs: false
			}
		}
	]
}
