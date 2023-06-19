const { DefaultButton, TooltipHost, ITooltipHostStyles, ThemeProvider, initializeIcons } = window.FluentUIReact;
const { useId } = window.FluentUIReactHooks;

const calloutProps = { gapSpace: 0 };
// The TooltipHost root uses display: inline by default.
// If that's causing sizing issues or tooltip positioning issues, try overriding to inline-block.
const hostStyles: Partial<ITooltipHostStyles> = { root: { display: 'inline-block' } };

const TooltipBasicExample: React.FunctionComponent = () => {
  // Use useId() to ensure that the ID is unique on the page.
  // (It's also okay to use a plain string and manually ensure uniqueness.)
  const tooltipId = useId('tooltip');

  let info: JSX.Element[] = [
    <a
      href={'https://www.microsoft.com'}
      target={'_blank'}
    >
      {'Help Link'}
    </a>
];

  return (
    <div>
      <TooltipHost
        // content="This is the tooltip content"
        tooltip-content={info}
        // This id is used on the tooltip itself, not the host
        // (so an element with this id only exists when the tooltip is shown)
        id={tooltipId}
        calloutProps={calloutProps}
        styles={hostStyles}
      >
        <DefaultButton aria-describedby={tooltipId}>Hover over me</DefaultButton>
      </TooltipHost>
    </div>
  );
};

const TooltipBasicExampleWrapper = () => <ThemeProvider><TooltipBasicExample /></ThemeProvider>;
ReactDOM.render(<TooltipBasicExampleWrapper />, document.getElementById('tooltip-content'));
