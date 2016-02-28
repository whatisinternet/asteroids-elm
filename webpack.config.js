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
    extensions: ['', '.js', '.elm', '.png', '.jpg']
  },

  module: {
    loaders: [
      {
        test: /\.elm$/,
        include: path.join(__dirname, 'src'),
        loader: 'elm-webpack'
      },
      {
        test: /\.(png|jpg)$/,
        include: path.join(__dirname, 'assets'),
        loader: "file-loader?name=[name].[ext]"
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
