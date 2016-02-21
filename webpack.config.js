var path = require('path')

module.exports = {
  entry: ['./src/index.js'],

  output: {
    path: './dist',
    publicPath: '/',
    filename: 'index.js'
  },

  resolve: {
    extensions: ['', '.js', '.elm']
  },

  module: {
    loaders: [
      {
        test: /\.elm$/,
        include: path.join(__dirname, 'src'),
        loader: 'elm-webpack'
      }
    ],

    noParse: /\.elm$/
  },

  devServer: {
    contentBase: './',
    stats: 'errors-only'
  }
};
