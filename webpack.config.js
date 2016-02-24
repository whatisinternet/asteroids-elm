var path = require('path'),
    webpack = require('webpack'),
    minimize = process.argv.indexOf('--minimize') !== -1,
    additionalPlugins = []

if (minimize) {
  additionalPlugins.push( new webpack.optimize.UglifyJsPlugin());
}

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
  },

  plugins: additionalPlugins
};
