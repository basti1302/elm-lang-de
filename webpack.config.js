// most of this setup is from the elm-webpack-starter example:
// https://github.com/moarwick/elm-webpack-starter/
var path              = require('path');
var webpack           = require('webpack');
var merge             = require('webpack-merge');
var HtmlWebpackPlugin = require('html-webpack-plugin');
var autoprefixer      = require('autoprefixer');
var CopyWebpackPlugin = require('copy-webpack-plugin');

// detemine build env
var TARGET_ENV =
  process.env.npm_lifecycle_event.indexOf('production') >= 0 ?
    'production' :
    'development';

console.log('TARGET_ENV:', TARGET_ENV);

// common webpack config
var commonConfig = {
  output: {
    path: path.resolve( __dirname, 'dist/'),
    filename: 'scripts/[hash].js',
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm'],
  },

  module: {
    noParse: /\.elm$/,
    loaders: [
      // This loader is required for font-awesome to properly load font files
      // referenced in font-awesome's css files. These are referenced like this
      // font-awesome/fonts/fontawesome-webfont.eot?v=4.7.0
      // and we need to stript the "?v=4.7.0" from the end.
      {
        test: /\.(woff2|woff|ttf|eot|svg)(\?v=[a-z0-9]\.[a-z0-9]\.[a-z0-9])?$/,
        loader: 'url-loader?limit=100000'
      },
      {
        test: /\.(woff|woff2|svg)$/,
        loader: 'file-loader',
      },
    ],
  },

  stylus: {
    import: [
      '../styles/lib/variables.styl',
      '../styles/lib/mediaQueries.styl',
    ]
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: 'frontend/index.html',
      inject:   'body',
      filename: 'index.html',
    }),
  ],

  postcss: [
    autoprefixer({ browsers: ['last 2 versions'] }),
  ],
};

// additional webpack settings for local env (when invoked by 'npm start')
if (TARGET_ENV === 'development') {
  module.exports = merge(commonConfig, {
    entry: [
      'webpack-dev-server/client?http://localhost:7000',
      path.join( __dirname, 'frontend/scripts/index.js' ), ],

    devServer: {
      port: 7000,
      inline: true,
      progress: true,
      proxy: [
        // redirect API requests to the Haskell backend
        {
          context: [
            '/api/**',
            '/oauth/**',
          ],
          target: 'http://localhost:8000',
        }
      ]
    },

    module: {
      loaders: [
        {
          test:    /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader:  'elm-hot!elm-webpack?verbose=true&warn=true&debug=true',
        },
        {
          test: /\.(css|styl)$/,
          loaders: [
            'style-loader',
            'css-loader',
            'postcss-loader',
            'stylus-loader',
          ],
        },
      ],
    }
  });
}

// additional webpack settings for prod env (when invoked via 'npm run build')
if (TARGET_ENV === 'production') {
  module.exports = merge(commonConfig, {
    entry: path.join( __dirname, 'frontend/scripts/index.js'),

    module: {
      loaders: [
        {
          test: /\.elm$/,
          exclude: [/elm-stuff/, /node_modules/],
          loader: 'elm-webpack',
        },
        {
          test: /\.(css|styl)$/,
          loaders: [
            'style-loader',
            'css-loader',
            'postcss-loader',
            'stylus-loader',
          ],
        },
      ],
    },

    plugins: [
      new CopyWebpackPlugin([
        { from: 'frontend/img', to: 'img' },
        { from: 'frontend/favicon.ico' }
      ]),

      new webpack.optimize.OccurenceOrderPlugin(),

      new webpack.optimize.UglifyJsPlugin({
        minimize: true,
        mangle: true,
        compressor: { warnings: false },
      }),
    ]
  });
}
