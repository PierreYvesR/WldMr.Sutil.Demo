// Template for webpack.config.js in Fable projects
// Find latest version in https://github.com/fable-compiler/webpack-config-template

// In most cases, you'll only need to edit the CONFIG object (after dependencies)
// See below if you need better fine-tuning of Webpack options

// Dependencies. Also required: core-js, sass, sass-loader, css-loader, style-loader, file-loader
var path = require('path');
const HtmlWebpackPlugin = require('html-webpack-plugin');
const CopyWebpackPlugin = require('copy-webpack-plugin');
const MonacoWebpackPlugin = require('monaco-editor-webpack-plugin');
const CompressionPlugin = require("compression-webpack-plugin");


// const TerserPlugin = require('terser-webpack-plugin');
// const BundleAnalyzerPlugin = require('webpack-bundle-analyzer').BundleAnalyzerPlugin;

var CONFIG = {
    // The tags to include the generated JS and CSS will be automatically injected in the HTML template
    // See https://github.com/jantimon/html-webpack-plugin
    indexHtmlTemplate: './src/App/index.html',
    fsharpEntry: './src/App/build/Program.js',
    cssEntry: './src/App/styles/main.scss',
    outputDir: './dist',
    assetsDir: './src/App/public',
    devServerPort: 8080
}

// If we're running the webpack-dev-server, assume we're in development mode
// var isProduction = !process.argv.find(v => v.indexOf('webpack-dev-server') !== -1);
// If we're running webpack serve, assume we're in development
var isProduction = !hasArg(/serve/);
var environment = isProduction ? 'production' : 'development';
if (isProduction)
  process.env.NODE_ENV = 'production'
var outputWebpackStatsAsJson = hasArg('--json');

if (!outputWebpackStatsAsJson) {
  console.log('Bundling for ' + environment);
}
  
// The HtmlWebpackPlugin allows us to use a template for the index.html page
// and automatically injects <script> or <link> tags for generated bundles.
var commonPlugins = [
    new HtmlWebpackPlugin({
        filename: 'index.html',
        template: resolve(CONFIG.indexHtmlTemplate)
    }),
    new MonacoWebpackPlugin({
      languages: [
          "json"
      ]
    })
];

module.exports = {
    // In development, bundle styles together with the code so they can also
    // trigger hot reloads. In production, put them in a separate CSS file.
    entry: isProduction ? {
        app: [resolve(CONFIG.fsharpEntry), resolve(CONFIG.cssEntry)]
    } : {
        app: [resolve(CONFIG.fsharpEntry)],
        style: [resolve(CONFIG.cssEntry)]
        },
    // Add a hash to the output file name in production
    // to prevent browser caching if code changes
    output: {
        path: resolve(CONFIG.outputDir),
        filename: isProduction ? '[name].[fullhash].js' : '[name].js'
    },
    mode: isProduction ? 'production' : 'development',
    devtool: isProduction ? 'source-map' : 'eval-source-map',
    // stats: "detailed",
    optimization: isProduction ? {
          // Split the code coming from npm packages into a different file.
          // 3rd party dependencies change less often, let the browser cache them.
          splitChunks: {
              cacheGroups: {
                  commons: {
                      test: /node_modules/,
                      name: "vendors",
                      chunks: "all"
                  }
              }
          },

          // this should be the default anyway for production
          // minimize: true,
          // minimizer: [new TerserPlugin()]
        }:{
        // required for hot reload to work
        runtimeChunk: 'single'
    },
    plugins: isProduction ?
        commonPlugins.concat([
            new CopyWebpackPlugin({
                patterns: [{
                  from: resolve(CONFIG.assetsDir) }
                ]
            }),
            new CompressionPlugin(),
            // new BundleAnalyzerPlugin(),
        ])
        : commonPlugins.concat([
        ]),
    resolve: {
        // See https://github.com/fable-compiler/Fable/issues/1490
        symlinks: false,
        alias: {
            'plotly.js/dist/plotly': path.join(__dirname, 'node_modules/plotly.js/dist/plotly-basic.min.js'),
        }
    },
    // Configuration for webpack-dev-server
    devServer: {
        static: resolve(CONFIG.assetsDir),
        port: CONFIG.devServerPort,
        proxy: CONFIG.devServerProxy,
        // hot: true, // default value
    },
    module: {
        rules: [
            {
                test: /\.js$/,
                enforce: "pre",
                use: ["source-map-loader"],
            },
            {
                test: /\.(sass|scss|css)$/,
                use: [
                    // isProduction
                    //     ? MiniCssExtractPlugin.loader
                    //     : 'style-loader',
                    'style-loader',
                    'css-loader',
                    'sass-loader',
                    // 'postcss-loader'
                ],
            },
            {
                test: /\.(png|jpg|jpeg|gif|svg|woff|woff2|ttf|eot)(\?.*)?$/,
                use: ['file-loader']
            }
        ]
    }
};

function resolve(filePath) {
    return path.isAbsolute(filePath) ? filePath : path.join(__dirname, filePath);
}

function hasArg(arg) {
  return arg instanceof RegExp
    ? process.argv.some(x => arg.test(x))
    : process.argv.indexOf(arg) !== -1;
}
