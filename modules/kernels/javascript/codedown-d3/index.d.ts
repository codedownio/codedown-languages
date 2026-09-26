declare module "codedown-d3" {
  interface RenderOptions {
    width?: number;
    height?: number;
  }

  type DrawFn = (d3: any, el: any, data: any, size: { width: number; height: number }) => void;

  /** Draw in the browser, keeping the chart interactive. */
  export function render(fn: DrawFn, data?: any, options?: RenderOptions): void;
  /** The HTML `render` would display, for embedding somewhere else. */
  export function renderToHtml(fn: DrawFn, data?: any, options?: RenderOptions): string;
  /** Draw in the kernel with jsdom and display the resulting SVG. */
  export function renderStatic(fn: DrawFn, data?: any, options?: RenderOptions): void;
  /** The SVG `renderStatic` would display. */
  export function renderStaticToSvg(fn: DrawFn, data?: any, options?: RenderOptions): string;
}
